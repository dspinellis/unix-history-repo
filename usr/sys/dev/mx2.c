#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/tty.h"
#include "../h/inode.h"
#define	KERNEL	1
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

#define	MIN(a,b)	((a<b)?a:b)
short	cmask[16]	={
	01,	02,	04,
	010,	020,	040,
	0100,	0200,	0400,
	01000,	02000,	04000,
	010000,	020000,	040000, 0100000
};

#define	HIQ	100
#define	LOQ	20
#define	FP	((struct file *)cp)



char mcdebugs[NDEBUGS];

/*
 * Timing cell
 */
int	mxdummy;
int	*MP	= &mxdummy;



struct group *
getmpx(dev)
dev_t dev;
{
register d;

	d = minor(dev);
	if (d >= NGROUPS) 
		return(NULL);
	return(groups[d]);
}




mxopen(dev, flag)
{
register struct group *gp;
register struct file *fp;
register struct chan *cp;
int	msg;
	gp = getmpx(dev);
	if (gp == NULL) {
	bad:
		u.u_error = ENXIO;
		return;
	}

	if (gp->g_state == COPEN) {
		gp->g_state = INUSE+ISGRP;
		return;
	}
	if (!(gp->g_state&INUSE)) 
		goto bad;
	fp = u.u_ofile[u.u_r.r_val1];
	if (fp->f_inode != gp->g_inode) 
		goto bad;
	if ((cp=addch(gp->g_inode, 0))==NULL)
		goto bad;
	cp->c_flags = XGRP;
	cp->c_ottyp = cp->c_ttyp = (struct tty *)cp;
	cp->c_line = cp->c_oline = mpxline;
	fp->f_flag |= FMPY;
	fp->f_flag |= FREAD+FWRITE;
	fp->f_un.f_chan = cp;
	if (gp->g_inode == mpxip) {
		plock(mpxip);
		mpxname(cp);
		msg = M_OPEN;
	} else
		msg = M_WATCH;
	scontrol(cp, msg, u.u_uid);
	sleep((caddr_t)cp,TTIPRI);
	if (cp->c_flags&NMBUF)
		prele(mpxip);
	if (cp->c_flags & WCLOSE) {
		chdrain(cp);
		chfree(cp);
		goto bad;
	}
	cp->c_fy = fp;
	cp->c_pgrp = u.u_procp->p_pgrp;
}


char	mxnmbuf[NMSIZE];
int	nmsize;
struct	chan *mxnmcp;
mpxname(cp)
register struct chan *cp;
{
register char *np;
register c;
	np = mxnmbuf;
	u.u_dirp = (caddr_t)u.u_arg[0];
	
	while (np < &mxnmbuf[NMSIZE]) {
		c = uchar();
		if (c <= 0)
			break;
		*np++ = c;
	}
	*np++ = '\0';
	nmsize = np - mxnmbuf;

	cp->c_flags |= NMBUF;
}


mxclose(dev, flag, cp)
dev_t	dev;
register struct chan *cp;
{
register struct group *gp;
register struct inode *ip;
register struct file *fp;
int	i, fmp;

	fmp = flag&FMP;

	/*
	 * close a channel
	 */
	if (cp!=NULL && fmp && fmp!=FMP) {
		for(fp=file; fp < &file[NFILE]; fp++) 
		    if(fp->f_count && fp->f_flag&FMP && fp->f_un.f_chan==cp){
				return;
			}
		chdrain(cp);
		if ((cp->c_flags&WCLOSE)==0) {
			scontrol(cp, M_CLOSE, 0);
			cp->c_flags |= WCLOSE;
		} else {
			chfree(cp);
		}
		return;
	}

	if ((gp = getmpx(dev)) == NULL)
		return;
	ip = gp->g_inode;
	if (ip==NULL || (ip->i_mode&IFMT)!=IFMPC) {
		return;
	}

	for(fp=file; fp < &file[NFILE]; fp++) {
		if (fp->f_count && (fp->f_flag&FMP)==FMP && fp->f_inode==ip) {
			return;
		}
	}

	if (ip == mpxip) {
		mpxip = NULL;
		prele(ip);
	}

	for(i=0;i<NINDEX;i++)
		if ((cp=gp->g_chans[i])!=NULL)
			detach(cp);

	groups[minor(dev)] = NULL;
	plock(ip);
	i = ip->i_mode;
	i &= ~IFMT;
	i |= IFCHR;
	ip->i_mode = i;
	zero((caddr_t)gp, sizeof (struct group));
	ip->i_flag |= IUPD|ICHG;
	iput(ip);
}

zero(s, cc)
register char *s;
register cc;
{
	while (cc--)
		*s++ = 0;
}

char	m_eot[] ={ M_EOT, 0, 0, 0};

/*
 * Mxread + mxwrite are entered from cdevsw
 * for all read/write calls.  Operations on
 * an mpx file are handled here.
 * Calls are made through linesw to handle actual
 * data movement.
 */
mxread(dev)
{
register struct group *gp;
register struct chan *cp;
register esc;
struct rh h;
caddr_t	base;
unsigned count;
int	s, xfr, more, fmp;

	if ((gp=getmpx(dev))==NULL) {
bad:
		u.u_error = ENXIO;
		return;
	}
	FP = getf(u.u_arg[0]);
	fmp = FP->f_flag & FMP;
	if (fmp != FMP) {
		msread(fmp, FP->f_un.f_chan);
		return;
	}

	if ((int)u.u_base & 1)
		goto bad;
	s = spl6();
	while (gp->g_datq == 0) {

		sleep((caddr_t)&gp->g_datq, TTIPRI);
	}

	while (gp->g_datq && u.u_count >= CNTLSIZ + 2) {
		splx(s);
		esc = 0;
		cp = nextcp(gp);
		if (cp==NULL) {
			continue;
		}
		h.index = cpx(cp);
		if (count = cp->c_ctlx.c_cc) {
			count += CNTLSIZ;
			if (cp->c_flags&NMBUF)
				count += nmsize;
			if (count > u.u_count) {
				sdata(cp);
				return;
			}
			esc++;
		}
		base = u.u_base;
		count = u.u_count;
		u.u_base += sizeof h;
		u.u_count -= sizeof h;
		xfr = u.u_count;
		if (esc && cp->c_flags&PORT) {
			more = mcread(cp);
		} else {
			more = (*linesw[cp->c_line].l_read)(cp->c_ttyp);
		}
		if (more > 0)
			sdata(cp);
		if (more < 0)
			scontrol(cp, M_CLOSE, 0);
		if (xfr == u.u_count) {
			esc++;
			iomove((caddr_t)m_eot, sizeof m_eot, B_READ);
		}
		xfr -= u.u_count;
		if (esc) {
			h.count = 0;
			h.ccount = xfr;
		} else {
			h.count = xfr;
			h.ccount = 0;
			mxrstrt(cp, &cp->cx.datq, BLOCK|ALT);
		}
		if (u.u_count && (xfr&1)) {
			u.u_base++;
			u.u_count--;
		}
		copyout((caddr_t)&h, base, sizeof h);

		s = spl6();
	}
}





mxwrite(dev)
{
register struct chan *cp;
struct	wh h;
struct group *gp;
int	ucount, esc, fmp, burpcount;
caddr_t	ubase, hbase;

	if ((gp=getmpx(dev))==NULL) {
		u.u_error = ENXIO;
		return;
	}
	FP = getf(u.u_arg[0]);
	fmp = FP->f_flag & FMP;
	if (fmp != FMP) {
		mswrite(fmp, FP->f_un.f_chan);
		return;
	}
	burpcount = 0;
	while (u.u_count >= sizeof h) {
		hbase = u.u_base;
		iomove((caddr_t)&h, sizeof h, B_WRITE);
		if (u.u_error)
			return;
		esc = 0;
		if (h.count==0) {
			esc++;
			h.count = h.ccount;
		}
		cp = xcp(gp, h.index);
		if (cp==NULL)  {
			continue;
		}
		ucount = u.u_count;
		ubase = u.u_base;
		u.u_count = h.count;
		u.u_base = h.data;

		if (esc==0) {
			struct tty *tp;
			caddr_t waddr;
			int line;

			if (cp->c_flags&PORT) {
				line = cp->c_line;
				tp = cp->c_ttyp;
			} else {
				line = cp->c_oline;
				tp = cp->c_ottyp;
			}
		loop:
			waddr = (caddr_t)(*linesw[line].l_write)(tp);
			if (u.u_count) {
				if (gp->g_state&ENAMSG) {
					burpcount++;
					cp->c_flags |= BLKMSG;
/*
					scontrol(cp, M_BLK, u.u_count);
*/
					h.ccount = -1;
					h.count = u.u_count;
					h.data = u.u_base;
					copyout((caddr_t)&h, hbase, sizeof h);
				} else {
					if (waddr==0) {
						u.u_error = ENXIO;
						return;
					}
					sleep(waddr, TTOPRI);
					goto loop;
				}
			}
		} else
			mxwcontrol(cp); 
		u.u_count = ucount;
		u.u_base = ubase;
	}
	u.u_count = burpcount;
}



/*
 * Mcread and mcwrite move data on an mpx file.
 * Transfer addr and length is controlled by mxread/mxwrite.
 * Kernel-to-Kernel and other special transfers are not
 * yet in.
 */
mcread(cp)
register struct chan *cp;
{
register struct clist *q;
register char *np;

int cc;

	q = (cp->c_ctlx.c_cc) ? &cp->c_ctlx : &cp->cx.datq;
	cc = mxmove(q, B_READ);

	if (cp->c_flags&NMBUF && q == &cp->c_ctlx) {
		np = mxnmbuf;
		while (nmsize--)
			passc(*np++);
		cp->c_flags &= ~NMBUF;
		prele(mpxip);
	}
	if (cp->c_flags&PORT)
		return(cp->c_ctlx.c_cc + cp->c_ttyp->t_rawq.c_cc); else
		return(cp->c_ctlx.c_cc + cp->cx.datq.c_cc);

}


char *
mcwrite(cp)
register struct chan *cp;
{
register struct clist *q;
register cc;
int	s;

	q = &cp->cy.datq;
	while (u.u_count) {
		s = spl6();
		if (q->c_cc > HIQ || (cp->c_flags&EOTMARK)) {
			cp->c_flags |= SIGBLK;
			splx(s);
			break;
		}
		splx(s);
		cc = mxmove(q, B_WRITE);
	}
	wakeup((caddr_t)q);
	return((caddr_t)q);
}


/*
 * Msread and mswrite move bytes
 * between user and non-multiplexed channel.
 */
msread(fmp, cp)
register struct chan *cp;
{
register struct clist *q;
int s;

	q = (fmp&FMPX) ? &cp->cx.datq : &cp->cy.datq;
	s = spl6();
	while (q->c_cc == 0) {
		if (cp->c_flags & EOTMARK) {
			cp->c_flags &= ~EOTMARK;
			if (cp->c_flags&ENAMSG)
				scontrol(cp, M_UBLK, 0);
			else {
				wakeup((caddr_t)cp);
				wakeup((caddr_t)q);
			}
			goto out;
		}
		if (cp->c_flags&WCLOSE) {
			u.u_error = ENXIO;
			goto out;
		}
		sleep((caddr_t)q,TTIPRI);
	}
	splx(s);
	while (mxmove(q, B_READ) > 0)
		;
	mxrstrt(cp, q, SIGBLK);
	return;
out:
	splx(s);
}


mswrite(fmp, cp)
register struct chan *cp;
{
register struct clist *q;
register int cc;

	q = (fmp&FMPX) ? &cp->cy.datq : &cp->cx.datq;
	while (u.u_count) {

		spl6();
		if (cp->c_flags&WCLOSE) {
		bad:
			signal(SIGPIPE, cp->c_pgrp);
			return;
		}

		while (q->c_cc>100) {
			if (cp->c_flags&WCLOSE)
				goto bad;
			sdata(cp);
			cp->c_flags |= BLOCK;
			sleep((caddr_t)q+1,TTOPRI);
		}
		spl0();
		cc = mxmove(q, B_WRITE);
		if (cc < 0)
			break;
	}
	if (fmp&FMPX) {
		if (cp->c_flags&YGRP) 
			sdata(cp); else
			wakeup((caddr_t)q);
	} else {
		if (cp->c_flags&XGRP) 
			sdata(cp); else
			wakeup((caddr_t)q);
	}
}



/*
 * move chars between clist and user space.
 */
mxmove(q, dir)
register struct clist *q;
register dir;
{
register cc;
char buf[HIQ];

	cc = MIN(u.u_count, sizeof buf);
	if (dir == B_READ) 
		cc = q_to_b(q, buf, cc);
	if (cc <= 0)
		return(cc);
	iomove((caddr_t)buf, cc, dir);
	if (dir == B_WRITE)
		cc = b_to_q(buf, cc, q);
	return(cc);
}



mxrstrt(cp, q, b)
register struct chan *cp;
register struct clist *q;
register b;
{
int s;

	s = spl6();
	if (cp->c_flags&b && q->c_cc<LOQ) {
		cp->c_flags &= ~b;
		if (b&ALT)
			wakeup((caddr_t)q+1); else
			mcstart(cp, (caddr_t)q);
	}
	if (cp->c_flags&WFLUSH)
		wakeup((caddr_t)q+2);
	splx(s);
}



/*
 * called from driver start or xint routines
 * to wakeup output sleeper.
 */
mcstart(cp, q)
register struct chan *cp;
register caddr_t q;
{

	if (cp->c_flags&(BLKMSG)) {
		cp->c_flags &= ~BLKMSG;
		scontrol(cp, M_UBLK, 0);
	} else
		wakeup((caddr_t)q);
}


mxwcontrol(cp)
register struct chan *cp;
{
short	cmd[2];
int	s;

	iomove((caddr_t)cmd, sizeof cmd, B_WRITE);
	switch(cmd[0]) {
	/*
	 * not ready to queue this up yet.
	 */
	case M_EOT:
		s = spl6();
		while (cp->c_flags & EOTMARK)
			if (cp->c_flags&ENAMSG) {
				scontrol(cp, M_BLK, 0);
				goto out;
			} else
				sleep((caddr_t)cp, TTOPRI);
		cp->c_flags |= EOTMARK;
	out:
		splx(s);
		break;
	case M_IOCTL:
printf("M_IOCTL");
		break;
	default:
		u.u_error = ENXIO;
	}
}








mxioctl(dev, cmd, addr, flag)
caddr_t addr;
{
struct group *gp;
int fmp;
struct file *fp;

	if ((gp = getmpx(dev)) == NULL) {
bad:
		u.u_error = ENXIO;
		return;
	}

	fp = getf(u.u_arg[0]);
	if (fp==NULL)
		goto bad;

	fmp = fp->f_flag & FMP;

	if (fmp == FMP) {
		switch(cmd) {
		case MXLSTN:
			if (mpxip == NULL) {
				mpxip = gp->g_inode;
			} else
				goto bad;
			break;
		case MXNBLK:
			gp->g_state |= ENAMSG;
			break;
		default:
			goto bad;
		}
	}
}




chdrain(cp)
register struct chan *cp;
{
register struct tty *tp;
int wflag;

	chwake(cp);

	wflag = (cp->c_flags&WCLOSE)==0;
	tp = cp->c_ttyp;
	if (tp == NULL)		/* prob not required */
		return;
	if (cp->c_flags&PORT && tp->t_chan == cp) {
		cp->c_ttyp = NULL;
		tp->t_chan = NULL;
		return;
	}
	if (wflag) 
		wflush(cp,&cp->cx.datq); else
		flush(&cp->cx.datq);
	if (!(cp->c_flags&YGRP)) {
		flush(&cp->cy.datq);
	}
}

chwake(cp)
register struct chan *cp;
{
register char *p;

	wakeup((caddr_t)cp);
	flush(&cp->c_ctlx);
	p = (char *)&cp->cx.datq;
	wakeup((caddr_t)p); wakeup((caddr_t)++p); wakeup((caddr_t)++p);
	p = (char *)&cp->cy.datq;
	wakeup((caddr_t)p); wakeup((caddr_t)++p); wakeup((caddr_t)++p);
}


chfree(cp)
register struct chan *cp;
{
register struct group *gp;
register i;

	gp = cp->c_group;
	if (gp==NULL)
		return;
	i = cp->c_index;
	if (cp == gp->g_chans[i]) {
		gp->g_chans[i] = NULL;
	}
	cp->c_group = NULL;
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
	if (sdata(cp) == NULL)
		return;
	putw(event,q);
	putw(value,q);
	splx(s);
}

sdata(cp)
register struct chan *cp;
{
register struct group *gp;
register short x;
register struct group *lgp;
int s;

	gp = cp->c_group;
	if (gp==NULL) {
		return(0);
	}
	x = cp->c_index;

	s = spl6();
	while (gp) {
		if ((gp->g_state&ISGRP)==0) {
			return(0);
		}
		gp->g_datq |= cmask[x];
		x = gp->g_index;
		lgp = gp;
		gp = gp->g_group;
	}
	gp =  lgp;
	splx(s);
	wakeup((caddr_t)&gp->g_datq);
	return((int)gp);
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
		if ((x&017) >= NINDEX)
			break;
		i++;
	}
	return((struct chan *)gp);
}

cpx(cp)
register struct chan *cp;
{
register x;
register struct group *gp;

	if (cp==NULL)
		return(-1);
	x = (-1<<4) + cp->c_index;
	gp = cp->c_group;
	if (gp==NULL || (gp->g_state&ISGRP)==0)
		return(-1);
	gp = gp->g_group;
	while (gp && gp->g_state&ISGRP) {
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
/*
prstuff(s,cc)
register char *s;
register cc;
{
	while (cc--)
		printf("%o ",*s++&0377);
}

prascii(s, cc)
register char *s;
register cc;
{
register c;
	while (cc--) {
		c = *s++;
		if (c>=040 && c<=0176)
			putchar(c); else
			printf(" %o ", c&0377);
	}
}
*/
