#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/tty.h"
#include "../h/inode.h"
#include "../h/mx.h"
#include "../h/file.h"
#include "../h/conf.h"
#include "../h/buf.h"

/*
 * multiplexor driver
 */
struct	chan	chans[NCHANS];
struct	group	*groups[NGROUPS];
int	mpxline;
struct chan *xcp();
struct chan *addch();
struct chan *nextcp();

int	chzero;
short	cmask[16]	={
	01,	02,	04,
	010,	020,	040,
	0100,	0200,	0400,
	01000,	02000,	04000,
	010000,	020000,	040000, 0100000
};

char mcdebugs[NDEBUGS];
mxopen(dev,flag)
{
register struct group *gp;
register struct file *fp;
register struct chan *cp;
int d;

struct inode *ip;
	d = minor(dev);
	if (d>=NGROUPS) {
	bad:
		u.u_error = ENXIO;
		printf("bad mxopen\n");
		return;
	}
	gp = groups[d];
	if (gp->g_state == COPEN) {
		gp->g_state = INUSE+ISGRP;
		return;
	}
	if (!(gp->g_state&INUSE)) 
		goto bad;
	fp = u.u_ofile[u.u_r.r_val1];
	if (fp->f_inode != gp->g_inode) 
		goto bad;
	if ((cp=addch(gp->g_inode))==NULL)
		goto bad;
	cp->c_flags = XGRP;
	cp->c_ottyp = cp->c_ittyp = (struct tty *)cp;
	cp->c_iline = cp->c_oline = mpxline;
	fp->f_flag |= FMPY;
	fp->f_flag |= FREAD+FWRITE;
	fp->f_un.f_chan = cp;
	cp->c_pgrp = u.u_procp->p_pgrp;
#ifdef CTRACE
	printf("open pgrp %d\n",cp->c_pgrp);
#endif
	scontrol(cp, M_WATCH+(cp->c_index<<8), u.u_uid);
	sleep((caddr_t)cp,TTIPRI);
	if (cp->c_flags & WCLOSE) {
		printf("WCLOSE on %o\n",cp);
		chfree(cp,0);
		goto bad;
	}
	cp->c_fy = fp;
}


mxclose(dev,cp)
dev_t	dev;
struct chan *cp;
{
register i;
register struct group *gp;
register struct inode *ip;

	gp = groups[minor(dev)];
	ip = gp->g_inode;
	if (ip==NULL) {
		if (cp==NULL || (int)cp==FWRITE)
		printf("bad close gp %o gpi %o ip %o ipg %o\n",
			gp,gp->g_inode,ip,ip->i_un.i_group);
		if (cp==NULL)
			return;
	}
	if (cp!=NULL && (int)cp!=FWRITE) {
		i = cp->c_index;
		if (!cp->c_flags&WCLOSE) {
			chfree(cp,1);
			scontrol(cp, M_CLOSE, 0);
printf("x1 close\n");
		} else {
			chfree(cp,0);
#ifdef CTRACE
			printf("WC close\n");
#endif
		}
		gp->g_chans[i] = NULL;
		cp->c_pgrp = 0;
		cp->c_group = NULL;
		cp->c_flags |= WCLOSE;
		return;
	}
	if ((ip->i_mode&IFMT)!=IFMPC) {
		printf("close on inode %o\n",ip);
		return;
	}
	for(i=0;i<NINDEX;i++) {
		if ((cp=gp->g_chans[i])!=NULL) {
			if (cp->c_flags&ISGRP) {
				((struct group *)cp)->g_state &= ~ISGRP;
				((struct group *)cp)->g_group = NULL;
				continue;
			}
			cp->c_flags |= WCLOSE;
			wakeup((caddr_t)cp);
			signal(cp->c_pgrp,SIGHUP);
			chfree(cp,0);
			if (gp->g_chans[i])
				printf("chfree didn't clear inode\n");
		}
	}
	gp->g_state = NULL;
	groups[minor(dev)] = NULL;
	i = ip->i_mode;
	i &= ~IFMT;
	i |= IFCHR;
	ip->i_mode = i;
	ip->i_flag |= IUPD|ICHG;
	iput(ip);
}


mxread(dev)
{
register struct group *gp;
struct clist *q;
struct chan *cp;
register i;
struct file *fp;
struct header h;
caddr_t	base;
unsigned count;
int	s,xfr,more;
int esc;

	i = minor(dev);
	if (i>=NGROUPS) {
		u.u_error = ENXIO;
		return;
	}
	gp = groups[i];
	fp = getf(u.u_arg[0]);
	if ((fp->f_flag&FMP)!=FMP) {
		msread(fp,gp);
		return;
	}

	s = spl6();
	while (gp->g_datq == 0) {

		sleep((caddr_t)&gp->g_datq, TTIPRI);
	}

	while (gp->g_datq && u.u_count >= CNTLSIZ) {
		splx(s);
		esc = 0;
		cp = nextcp(gp);
		if (cp==NULL) {
			continue;
		}
		if (cp->c_ctlx.c_cc)
			esc = 2;
		base = u.u_base;
		count = u.u_count;
		u.u_base += HDRSIZE + esc;
		u.u_count -= HDRSIZE + esc;
		xfr = u.u_count;
		more = (*linesw[cp->c_iline].l_read)(cp->c_ittyp);
		if (esc || more > 0) {
			sdata(cp);
		}
		if (more<0) {
printf("m<0\n");
printf("x2 close\n");
			scontrol(cp, M_CLOSE, 0);
		}
		xfr -= u.u_count;
		if (xfr==0) {
			u.u_base = base;
			u.u_count = count;
			chzero++;
			continue;
		}
		h.index = cpx(cp);
		if (esc) {
			h.count = 0;
			h.ccount = xfr;
		} else
			h.count = xfr;
		if (xfr&1) {
			u.u_base++;
			u.u_count--;
		}
		copyout(&h, base, HDRSIZE+esc);

		q = &cp->cx.datq;
		if (cp->c_flags&BLOCK && esc==0) {
			if (q->c_cc<25) {
				wakeup((caddr_t)q+1);
				cp->c_flags &= ~BLOCK;
			} else {
			}
		}
		if (cp->c_flags&WFLUSH)
			wakeup((caddr_t)q+2);
		s = spl6();
	}
}


mcread(cp)
register struct chan *cp;
{
register struct clist *q;
register c;


	if (cp->c_ctlx.c_cc)
		q = &cp->c_ctlx; else
		q = &cp->cx.datq;

	while( (c=getc(q)) >= 0) {
		passc(c);
	}
	return(q->c_cc);

}

msread(fp, gp)
struct file *fp;
struct group *gp;
{
register struct clist *q;
register struct chan *cp;
register i;

	cp = fp->f_un.f_chan;
	q = (fp->f_flag&FMPX) ? &cp->cx.datq : &cp->cy.datq;
	i = spl6();
	if (!q->c_cc) {
		if (cp->c_flags&WCLOSE) {
			printf("ms sees it %o\n",cp);
			u.u_error = ENXIO;
			return;
		}
		sleep((caddr_t)q,TTIPRI);
	}
	splx(i);
	i = 0;
	while(u.u_count && q->c_cc) {
		passc(getc(q));
		i++;
	}
	if (cp->c_flags&SIGBLK && q->c_cc < 20) {
		cp->c_flags &= ~SIGBLK;
		if (cp->c_flags&ENAMSG) 
			scontrol(cp, M_UBLK, 0); else
			wakeup((caddr_t)q);
	}
	if (cp->c_flags&WFLUSH)
		wakeup((caddr_t)q+2);
}


mxwrite(dev)
{
register i;
register struct chan *cp;
struct	header h;
struct file *fp;
struct group *gp;
int	ucount;
caddr_t	ubase;

	i = minor(dev);
	if (i>=NGROUPS) {
		u.u_error = ENXIO;
		return;
	}
	gp = groups[i];
	fp = getf(u.u_arg[0]);
	if ((fp->f_flag&FMP)!=FMP) {
		ucount = mswrite(fp,gp);
		return;
	}
	while (u.u_count) {
		iomove(&h, sizeof(struct header), B_WRITE);
/*
		if (count==0) {
			esc++;
			h.count = h.ccount;
		}
*/
		cp = xcp(gp, h.index);
		if (cp==NULL)  {
printf("nullo %o %o\n",cp,chans);
			u.u_count -= h.count;
			u.u_base += h.count;
			continue;
		}
		ucount = u.u_count;
		ubase = u.u_base;
		u.u_count = h.count;
		u.u_base = h.addr;
		(*linesw[cp->c_oline].l_write)(cp->c_ottyp);
		u.u_count = ucount;
		u.u_base = ubase;
	}
}


mcwrite(cp)
register struct chan *cp;
{
register struct clist *q;
register c;
int	s;

	q = &cp->cy.datq;

	while ((c=cpass())>=0) {
		s = spl6();
		while (q->c_cc > 100) {
			cp->c_flags |= SIGBLK;
			splx(s);
			if (cp->c_flags&ENAMSG) {
				scontrol(cp, M_BLK, (short)u.u_count);
				wakeup((caddr_t)q);
				while (cpass()>=0);
				return;
			} else
				sleep((caddr_t)q, TTOPRI);

		}
		splx(s);
		putc(c, q);
	}
	wakeup((caddr_t)q);
}


mcttwrite(tp,cc)
register struct tty *tp;
register cc;
{
register c;
struct chan *cp;

	if ((tp->t_state&CARR_ON)==0)
		return;
	while (cc && (c=cpass())>=0) {
		spl5();
		if (tp->t_outq.c_cc > 10) {
			ttstart(tp);
			spl0();
			cp = tp->t_chan;
			cp->c_flags |= SIGBLK;
			scontrol(cp, M_BLK, cc);
			while (cc-- && cpass()>=0);
			return;
		}
		spl0();
		ttyoutput(c,tp);
		cc--;
	}
	ttstart(tp);
}
mcttstart(tp)
struct tty *tp;
{
register struct chan *cp;

	cp = tp->t_chan;
	if (cp->c_flags&(BLKMSG+ENAMSG)) {
		cp->c_flags &= ~BLKMSG;
		scontrol(cp, M_UBLK, 0);
	}
}
mswrite(fp,gp)
struct file *fp;
struct group *gp;
{
register struct clist *q;
struct chan *cp;
register c;
int cc;

	cp = fp->f_un.f_chan;
	q = (fp->f_flag&FMPX) ? &cp->cy.datq : &cp->cx.datq;
	cc = 0;
	while((c=cpass())>=0) {
		spl6();
		if (cp->c_flags&WCLOSE) {
		bad:
			u.u_error = ENXIO;
			printf("ms write sees it %o\n",cp);
			return(cc);
		}
		while (q->c_cc>100) {
			if (cp->c_flags&WCLOSE)
				goto bad;
			sdata(cp);
/*
			if (q->c_cc) {
				printf("choops\n");
				sdata(cp);
				printf("sent\n");
			}
*/
			cc = 0;
			cp->c_flags |= BLOCK;
			sleep((caddr_t)q+1,TTOPRI);
		}
		spl0();
		if (putc(c,q)>=0)
			cc++; else
			printf("qe\n");
	}
	if (fp->f_flag&FMPX) {
		if (cp->c_flags&YGRP) 
			sdata(cp); else
			wakeup((caddr_t)q);
	} else {
		if (cp->c_flags&XGRP) 
			sdata(cp); else
			wakeup((caddr_t)q);
	}
	return(cc);
}



mxioctl(dev, cmd, addr, flag)
caddr_t addr;
{
/*
	u.u_error = ENOTTY;
*/
}




chfree(cp, flag)
register struct chan *cp;
{
register struct tty *tp;
struct group *gp;
struct inode *ip;

	if (chclear(cp))
		goto out;
	tp = cp->c_ittyp;
	if (tp->t_chan == cp) {
		cp->c_ittyp = NULL;
		tp->t_chan = NULL;
		if (flag && cp->c_iline==0)
			wflushtty(tp); else
			flushtty(tp);
	}
	if (flag) 
		wflush(cp,&cp->cx.datq); else
		flush(&cp->cx.datq);
	if (!(cp->c_flags&YGRP)) {
		flush(&cp->cy.datq);
	}
	cp->c_flags = NULL;
out:
	if (!flag) {
		gp = cp->c_group;
		cp->c_group = NULL;
		ip = gp->g_inode;
		if (ip==NULL || (ip->i_mode&IFMT)!=IFMPC) {
			printf("chfree on %o\n",ip);
			return;
		}
		gp->g_chans[cp->c_index] = NULL;
	}
}

chclear(cp)
register struct chan *cp;
{
register char *p;

#ifdef CTRACE
register struct file *fp;
	fp = cp->c_fy;
	if (fp) {
		printf(" count %d on cp %o\n",fp->f_count,cp);
	}
#endif
	p = (char *)&cp->cx.datq;
	wakeup(p); wakeup(++p); wakeup(++p);
	p = (char *)&cp->cy.datq;
	wakeup(p); wakeup(++p); wakeup(++p);
	return(0);
}



flush(q)
register struct clist *q;
{

	while(q->c_cc)
		getc(q);
}


wflush(cp,q)
register struct chan *cp;
register struct clist *q;
{
register s;

	s = spl6();
	while(q->c_cc) {
		if (cp->c_flags & WCLOSE) {
			flush(q);
			goto out;
		}
		cp->c_flags |= WFLUSH;
		sdata(cp);
		sleep((caddr_t)q+2,TTOPRI);
	}
out:
	cp->c_flags &= ~WFLUSH;
	splx(s);
}


scontrol(cp,event,value)
register struct chan *cp;
short event,value;
{
register struct clist *q;
int s;

	q = &cp->c_ctlx;
	s = spl6();
	putw(event,q);
	putw(value,q);
	splx(s);
	sdata(cp);
}

sdata(cp)
register struct chan *cp;
{
register struct group *gp;
register short x;
register struct group *lgp;
int s;

	gp = cp->c_group;
	x = cp->c_index;

	s = spl6();
	while (gp) {
		gp->g_datq |= cmask[x];
		x = gp->g_index;
		lgp = gp;
		gp = gp->g_group;
	}
	gp =  lgp;
	splx(s);
	wakeup((caddr_t)&gp->g_datq);
}



struct chan *
xcp(gp, x)
register struct group *gp;
register short x;
{
register i;

	i = 0;
	while (i<NLEVELS && gp->g_state&ISGRP) {
		gp = (struct group *)gp->g_chans[x&017];
		x >>= 4;
	}
	return((struct chan *)gp);
}

cpx(cp)
register struct chan *cp;
{
register x;
register struct group *gp;

	x = cp->c_index;
	gp = cp->c_group;
	gp = gp->g_group;
	while (gp) {
		x <<= 4;
		x |= gp->g_index;
		gp = gp->g_group;
	}
	return(x);
}



struct chan *
nextcp(gp)
register struct group *gp;
{

	if (gp->g_datq == 0) {
		gp = NULL;
		goto out;
	}

	while (gp != NULL && gp->g_state&ISGRP) {
		while ( (gp->g_datq & gp->g_rotmask) == 0) {
			gp->g_rot++;
			gp->g_rot &= 017;
			if (gp->g_rot)
				gp->g_rotmask <<= 1; else
				gp->g_rotmask = 1;
		}
		gp = (struct group *)gp->g_chans[gp->g_rot];
	}
	if (gp)
		rmdata(gp);
out:
	return((struct chan *)gp);
}

rmdata(cp)
register struct chan *cp;
{
register struct group *gp;
register short x;

	gp = cp->c_group;
	x = cp->c_index;

	while (gp) {
		gp->g_datq &= ~cmask[x];
		if (gp->g_datq)
			return;
		x = gp->g_index;
		gp = gp->g_group;
	}
}





mcrint(c, tp)
struct tty *tp;
{
}

mcxint(tp)
struct tty *tp;
{
}
prstuff(s,cc)
register char *s;
register cc;
{
	while (cc--)
		printf("%o ",*s++&0377);
}
