#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/proc.h"
#include "../h/tty.h"
#include "../h/inode.h"
#include "../h/mx.h"
#include "../h/file.h"
#include "../h/conf.h"

struct	chan	chans[NCHANS];
struct	group	*groups[NGROUPS];
int	mpxline;
struct chan *xcp();
dev_t	mpxdev	= -1;


char	mcdebugs[NDEBUGS];


struct	chan *
challoc(index)
register index;
{
register s;
register struct chan *cp;

	s = spl6();
	for(cp=chans; cp< &chans[NCHANS]; cp++)
		if(cp->c_group==NULL) {
			cp->c_index = index;
			cp->c_pgrp = 0;
			splx(s);
			return(cp);
		}
	splx(s);
	return(NULL);
}


gpalloc()
{
register i,s;

	s = spl6();
	for(i = NGROUPS-1; i>=0; i--)
		if (groups[i]==NULL) {
			groups[i]++;
			break;
		}
	splx(s);
	return(i);
}

struct chan *
addch(ip)
struct inode *ip;
{
register struct chan *cp;
register struct group *gp;
register i;

	plock(ip);
	gp = &ip->i_un.i_group;
	for(i=0;i<NINDEX;i++) {
		cp = (struct chan *)gp->g_chans[i];
		if (cp == NULL) {
			if ((cp=challoc(i)) != NULL) {
				gp->g_chans[i] = cp;
				cp->c_group = gp;
			}
			break;
		}
		cp = NULL;
	}
	prele(ip);
	return(cp);
}




/*
 * mpxchan system call
 */
mpxchan()
{
struct	inode *ip,*gip;
register int i;
extern	mxopen(), mcread();
extern	struct chan *addch();
dev_t	dev;
struct	tty *tp;
struct	file *fp,*chfp,*gfp;
struct	chan *cp;
struct	group *gp;

	/*
	 * common setup code.
	 */
	i = u.u_arg[3];
	gp = NULL;
	switch(i) {
	case NPGRP:
		if (u.u_arg[1] < 0)
			goto sw;
	case CHAN:
	case JOIN:
	case EXTR:
	case ATTACH:
	case DETACH:
	case CSIG:
		gfp = getf(u.u_arg[1]);
		if (gfp==NULL)
			goto bad;
		gip = gfp->f_inode;
		gp = &gip->i_un.i_group;
		if (gp->g_inode != gip)
			goto bad;
	}

sw:
	switch(i) {
	/*
	 * creat a null group
	 * return a file descriptor
	 */
	case MPX:
	case MPXN:
		if (mpxdev < 0) {
			for(i=0; linesw[i].l_open; i++) 
				if (linesw[i].l_read == mcread) {
					mpxline = i;
					goto found1;
				}
			goto bad;

		found1:
			for(i=0; cdevsw[i].d_open; i++) {
				if (cdevsw[i].d_open == mxopen)
					goto found2;
			}
		bad:
			u.u_error = ENXIO;
			return;
		found2:
			mpxdev = (dev_t)(i<<8);
			i = u.u_arg[3];
		}
		if (i==MPXN) {
			if ((ip=ialloc(rootdev))==NULL)
				goto bad;
			ip->i_mode = (u.u_arg[1]&0777)+IFCHR;
			ip->i_flag = IACC|IUPD|ICHG;
			ip->i_nlink = 1;
			goto merge;
		}
		ip = namei(uchar,1);
		if (ip != NULL) {
			u.u_error = EEXIST;
			iput(ip);
			return;
		}
		if (u.u_error)
			return;
		ip = maknode((u.u_arg[1]&0777)+IFCHR);
		if (ip == NULL)
			return;
	merge:
		if ((i = gpalloc()) < 0) {
			iput(ip);
			goto bad;
		}
		ip->i_un.i_rdev = (daddr_t)(mpxdev+i);
		gp = &ip->i_un.i_group;
		groups[i] = gp;
		gp->g_inode = ip;
		gp->g_state = COPEN;
		gp->g_group = NULL;
		gp->g_index = 0;
		gp->g_rotmask = 1;
		gp->g_rot = 0;
		gp->g_datq = 0;
		open1(ip,FREAD+FWRITE,2);
		if (u.u_error) {
			groups[i] = NULL;
			iput(ip);
			goto bad;
		}
		ip->i_mode |= IFMPC;
		ip->i_count++;
		fp = u.u_ofile[u.u_r.r_val1];
		fp->f_flag |= FMP;
		fp->f_un.f_chan = NULL;
		for(i=0;i<NINDEX;)
			gp->g_chans[i++] = NULL;
		return;
	/*
	 * join file descriptor (arg 0) to group (arg 1)
	 * return channel number
	 */
	case JOIN:
		if ((fp=getf(u.u_arg[0]))==NULL)
			goto bad;
		ip = fp->f_inode;
		i = ip->i_mode & IFMT;
		if (i == IFMPC) {
			mlink(fp->f_inode, gp);
			return;
		}
		if (i != IFCHR) 
			goto bad;
		dev = (dev_t)ip->i_un.i_rdev;
		tp = &cdevsw[major(dev)].d_ttys[minor(dev)];
		if (tp==NULL || tp->t_chan)
			goto bad;
		if ((cp=addch(gip))==NULL)
			goto bad;
		tp->t_chan = cp;
		cp->c_ottyp = cp->c_ittyp = tp;
		cp->c_iline = cp->c_oline = tp->t_line;
		cp->c_flags = XGRP+TTYO;
		u.u_r.r_val1 = cp->c_index;
		return;
	/*
	 * attach channel (arg 0) to group (arg 1)
	 */
	case ATTACH:
		cp = xcp(gp, u.u_arg[0]);
		if (cp==NULL)
			goto bad;
		u.u_r.r_val1 = cpx(cp);
		wakeup((caddr_t)cp);
		return;
	case DETACH:
		cp = xcp(gp, u.u_arg[0]);
		if (cp==NULL)
			goto bad;
		cp->c_flags |= WCLOSE;
		wakeup((caddr_t)cp);
		return;
	/*
	 * extract channel (arg 0) from group (arg 1)
	 * using X side (arg 2 zero) otherwise Y side
	 */
	case EXTR:
		cp = xcp(gp, u.u_arg[0]);
		if (cp==NULL) {
			goto bad;
		}
		if ((fp = falloc()) == NULL) {
			return;
		}
		fp->f_inode = gip;
		fp->f_un.f_chan = cp;
		fp->f_flag = (u.u_arg[2]) ? (FREAD+FWRITE+FMPY) : (FREAD+FWRITE+FMPX);
		return;
	/*
	 * make new chan on group (arg 1)
	 */
	case CHAN:
		if ((cp=addch(gip))==NULL)
			goto bad;
		cp->c_flags = XGRP;
		cp->c_ittyp = cp->c_ottyp = (struct tty *)cp;
		cp->c_iline = cp->c_oline = mpxline;
		u.u_r.r_val1 = cp->c_index;
		return;
	/*
	 * connect fd (arg 0) to channel fd (arg 1)
	 * (arg 2 <  0) => fd to chan only
	 * (arg 2 >  0) => chan to fd only
	 * (arg 2 == 0) => both directions
	 */
	case CONNECT:
		if ((fp=getf(u.u_arg[0]))==NULL)
			goto bad;
		if ((chfp=getf(u.u_arg[1]))==NULL)
			goto bad;
		ip = fp->f_inode;
		i = ip->i_mode&IFMT;
		if (i!=IFMPC)
			goto bad;
		dev = (dev_t)ip->i_un.i_rdev;
		tp = &cdevsw[major(dev)].d_ttys[minor(dev)];
		if (tp==NULL)
			goto bad;
		if (!(chfp->f_flag&FMPY)) {
			goto bad;
		}
		cp = chfp->f_un.f_chan;
		if (cp==NULL) {
			goto bad;
		}
		i = u.u_arg[2];
		if (i>=0) {
			cp->c_ottyp = tp;
			cp->c_oline = tp->t_line;
		}
		if (i<=0)  {
			tp->t_chan = cp;
			cp->c_ittyp = tp;
			cp->c_iline = tp->t_line;
		}
		return;
	case NPGRP: {
		register struct proc *pp;

		if (gp != NULL) {
			cp = xcp(gp, u.u_arg[0]);
			if (cp==NULL)
				goto bad;
		}
		pp = u.u_procp;
		pp->p_pgrp = pp->p_pid;
		if (u.u_arg[2])
			pp->p_pgrp = u.u_arg[2];
		if (gp != NULL)
			cp->c_pgrp = pp->p_pgrp;
		return;
	}
	case CSIG:
		cp = xcp(gp, u.u_arg[0]);
		if (cp==NULL)
			goto bad;
		signal(cp->c_pgrp, u.u_arg[2]);
		return;
	case DEBUG:
		i = u.u_arg[0];
		if (i<0 || i>NDEBUGS)
			return;
		mcdebugs[i] = u.u_arg[1];
		if (i==ALL)
			for(i=0;i<NDEBUGS;i++)
				mcdebugs[i] = u.u_arg[1];
		return;
	default:
		goto bad;
	}
}



mlink(sub,master)
struct group *sub, *master;
{
register i;


	for(i=0;i<NINDEX;i++) {
		if (master->g_chans[i] != NULL)
			continue;
		master->g_chans[i] = (struct chan *)sub;
		sub->g_group = master;
		sub->g_index = i;
		u.u_r.r_val1 = i;
		return;
	}
	u.u_error = ENXIO;
	return;
}
