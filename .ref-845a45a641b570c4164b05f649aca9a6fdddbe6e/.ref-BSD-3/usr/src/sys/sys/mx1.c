/*	mx1.c	2.1	1/5/80	*/

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

/*
 * Multiplexor:   clist version
 *
 * installation:
 *	requires a line in cdevsw -
 *		mxopen, mxclose, mxread, mxwrite, mxioctl, 0,
 *
 *	also requires a line in linesw -
 *		mxopen, mxclose, mcread, mcwrite, mxioctl, nulldev, nulldev,
 *
 *	The linesw entry for mpx should be the last one in the table.
 *	'nldisp' (number of line disciplines) should not include the
 *	mpx line.  This is to prevent mpx from being enabled by an ioctl.

 *	mxtty.c must be loaded instead of tty.c so that certain
 *	sleeps will not be done if a typewriter is connected to
 *	a channel and so that sdata will be called from ttyinput.
 *	
 */
struct	chan	chans[NCHANS];
struct	schan	schans[NPORTS];
struct	group	*groups[NGROUPS];
int	mpxline;
struct chan *xcp();
dev_t	mpxdev	= -1;


char	mcdebugs[NDEBUGS];


/*
 * Allocate a channel, set c_index to index.
 */
struct	chan *
challoc(index, isport)
{
register s,i;
register struct chan *cp;

	s = spl6();
	for(i=0;i<((isport)?NPORTS:NCHANS);i++) {
		cp = (isport)? (struct chan *)(schans+i): chans+i;
		if(cp->c_group == NULL) {
			cp->c_index = index;
			cp->c_pgrp = 0;
			cp->c_flags = 0;
			splx(s);
			return(cp);
		}
	}
	splx(s);
	return(NULL);
}



/*
 * Allocate a group table cell.
 */
gpalloc()
{
	register i;

	for (i=NGROUPS-1; i>=0; i--)
		if (groups[i]==NULL) {
			groups[i]++;
			return(i);
		}
	u.u_error = ENXIO;
	return(i);
}


/*
 * Add a channel to the group in
 * inode ip.
 */
struct chan *
addch(ip, isport)
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
			if ((cp=challoc(i, isport)) != NULL) {
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
 * Mpxchan system call.
 */

mpxchan()
{
	extern	mxopen(), mcread(), sdata(), scontrol();
	struct	inode	*ip, *gip;
	struct	tty	*tp;
	struct	file	*fp, *chfp, *gfp;
	struct	chan	*cp;
	struct	group	*gp, *ngp;
	struct	mx_args	vec;
	struct	a	{
		int	cmd;
		int	*argvec;
	} *uap;
	dev_t	dev;
	register int i;

	/*
	 * Common setup code.
	 */

	uap = (struct a *)u.u_ap;
	VOID copyin((caddr_t)uap->argvec, (caddr_t)&vec, sizeof vec);
	gp = NULL; gfp = NULL; cp = NULL;

	switch(uap->cmd) {

	case NPGRP:
		if (vec.m_arg[1] < 0)
			break;
	case CHAN:
	case JOIN:
	case EXTR:
	case ATTACH:
	case DETACH:
	case CSIG:
		gfp = getf(vec.m_arg[1]);
		if (gfp==NULL)
			return;
		gip = gfp->f_inode;
		gp = &gip->i_un.i_group;
		if (gp->g_inode != gip) {
			u.u_error = ENXIO;
			return;
		}
	}

	switch(uap->cmd) {

	/*
	 * Create an MPX file.
	 */

	case MPX:
	case MPXN:
		if (mpxdev < 0) {
			for (i=0; linesw[i].l_open; i++) {
				if (linesw[i].l_read==mcread) {
					mpxline = i;
					for (i=0; cdevsw[i].d_open; i++) {
						if (cdevsw[i].d_open==mxopen) {
							mpxdev = (dev_t)(i<<8);
						}
					}
				}
			}
			if (mpxdev < 0) {
				u.u_error = ENXIO;
				return;
			}
		}
		if (uap->cmd==MPXN) {
			if ((ip=ialloc(pipedev))==NULL)
				return;
			ip->i_mode = ((vec.m_arg[1]&0777)+IFMPC) & ~u.u_cmask;
			ip->i_flag = IACC|IUPD|ICHG;
		} else {
			u.u_dirp = vec.m_name;
			ip = namei(uchar,1);
			if (ip != NULL) {
				i = ip->i_mode&IFMT;
				u.u_error = EEXIST;
				if (i==IFMPC || i==IFMPB) {
					i = minor(ip->i_un.i_rdev);
					gp = groups[i];
					if (gp && gp->g_inode==ip)
						u.u_error = EBUSY;
				}
				iput(ip);
				return;
			}
			if (u.u_error)
				return;
			ip = maknode((vec.m_arg[1]&0777)+IFMPC);
			if (ip == NULL)
				return;
		}
		if ((i=gpalloc()) < 0) {
			iput(ip);
			return;
		}
		if ((fp=falloc()) == NULL) {
			iput(ip);
			groups[i] = NULL;
			return;
		}
		ip->i_un.i_rdev = (daddr_t)(mpxdev+i);
		ip->i_count++;
		prele(ip);

		gp = &ip->i_un.i_group;
		groups[i] = gp;
		gp->g_inode = ip;
		gp->g_state = INUSE|ISGRP;
		gp->g_group = NULL;
		gp->g_file = fp;
		gp->g_index = 0;
		gp->g_rotmask = 1;
		gp->g_rot = 0;
		gp->g_datq = 0;
		for(i=0;i<NINDEX;)
			gp->g_chans[i++] = NULL;

		fp->f_flag = FREAD|FWRITE|FMP;
		fp->f_inode = ip;
		fp->f_un.f_chan = NULL;
		return;

	/*
	 * join file descriptor (arg 0) to group (arg 1)
	 * return channel number
	 */

	case JOIN:
		if ((fp=getf(vec.m_arg[0]))==NULL)
			return;
		ip = fp->f_inode;
		switch (ip->i_mode & IFMT) {

		case IFMPC:
			if ((fp->f_flag&FMP) != FMP) {
				u.u_error = ENXIO;
				return;
			}
			ngp = &ip->i_un.i_group;
			if (mtree(ngp, gp) == NULL)
				return;
			fp->f_count++;
			u.u_r.r_val1 = cpx(ngp);
			return;

		case IFCHR:
			dev = (dev_t)ip->i_un.i_rdev;
			tp = cdevsw[major(dev)].d_ttys;
			if (tp==NULL) {
				u.u_error = ENXIO;
				return;
			}
			tp = &tp[minor(dev)];
			if (tp->t_chan) {
				u.u_error = ENXIO;
				return;
			}
			if ((cp=addch(gip, 1))==NULL) {
				u.u_error = ENXIO;
				return;
			}
			tp->t_chan = cp;
			cp->c_fy = fp;
			fp->f_count++;
			cp->c_ttyp = tp;
			cp->c_line = tp->t_line;
			cp->c_flags = XGRP+PORT;
			u.u_r.r_val1 = cpx(cp);
			return;

		default:
			u.u_error = ENXIO;
			return;

		}

	/*
	 * Attach channel (arg 0) to group (arg 1).
	 */

	case ATTACH:
		cp = xcp(gp, vec.m_arg[0]);
		if (cp==NULL || cp->c_flags&ISGRP) {
			u.u_error = ENXIO;
			return;
		}
		u.u_r.r_val1 = cpx(cp);
		wakeup((caddr_t)cp);
		return;

	case DETACH:
		cp = xcp(gp, vec.m_arg[0]);
		if (cp==NULL) {
			u.u_error = ENXIO;
			return;
		}
		detach(cp);
		return;

	/*
	 * Extract channel (arg 0) from group (arg 1).
	 */

	case EXTR:
		cp = xcp(gp, vec.m_arg[0]);
		if (cp==NULL) {
			u.u_error = ENXIO;
			return;
		}
		if (cp->c_flags & ISGRP) {
			VOID mxfalloc(((struct group *)cp)->g_file);
			return;
		}
		if ((fp=cp->c_fy) != NULL) {
			VOID mxfalloc(fp);
			return;
		}
		if ((fp=falloc()) == NULL)
			return;
		fp->f_inode = gip;
		gip->i_count++;
		fp->f_un.f_chan = cp;
		fp->f_flag = (vec.m_arg[2]) ?
				(FREAD|FWRITE|FMPY) : (FREAD|FWRITE|FMPX);
		cp->c_fy = fp;
		return;

	/*
	 * Make new chan on group (arg 1).
	 */

	case CHAN:
		if((gfp->f_flag&FMP)==FMP)cp = addch(gip, 0);
			if(cp == NULL){
			u.u_error = ENXIO;
			return;
			}
		cp->c_flags = XGRP;
		cp->c_fy = NULL;
		cp->c_ttyp = cp->c_ottyp = (struct tty *)cp;
		cp->c_line = cp->c_oline = mpxline;
		u.u_r.r_val1 = cpx(cp);
		return;

	/*
	 * Connect fd (arg 0) to channel fd (arg 1).
	 * (arg 2 <  0) => fd to chan only
	 * (arg 2 >  0) => chan to fd only
	 * (arg 2 == 0) => both directions
	 */

	case CONNECT:
		if ((fp=getf(vec.m_arg[0]))==NULL)
			return;
		if ((chfp=getf(vec.m_arg[1]))==NULL)
			return;
		ip = fp->f_inode;
		i = ip->i_mode&IFMT;
		if (i!=IFCHR) {
			u.u_error = ENXIO;
			return;
		}
		dev = (dev_t)ip->i_un.i_rdev;
		tp = cdevsw[major(dev)].d_ttys;
		if (tp==NULL) {
			u.u_error = ENXIO;
			return;
		}
		tp = &tp[minor(dev)];
		if (!(chfp->f_flag&FMPY)) {
			u.u_error = ENXIO;
			return;
		}
		cp = chfp->f_un.f_chan;
		if (cp==NULL || cp->c_flags&PORT) {
			u.u_error = ENXIO;
			return;
		}
		i = vec.m_arg[2];
		if (i>=0) {
			cp->c_ottyp = tp;
			cp->c_oline = tp->t_line;
		}
		if (i<=0)  {
			tp->t_chan = cp;
			cp->c_ttyp = tp;
			cp->c_line = tp->t_line;
		}
		u.u_r.r_val1 = 0;
		return;

	case NPGRP: {
		register struct proc *pp;

		if (gp != NULL) {
			cp = xcp(gp, vec.m_arg[0]);
			if (cp==NULL) {
				u.u_error = ENXIO;
				return;
			}
		}
		pp = u.u_procp;
		pp->p_pgrp = pp->p_pid;
		if (vec.m_arg[2])
			pp->p_pgrp = vec.m_arg[2];
		if (gp != NULL)
			cp->c_pgrp = pp->p_pgrp;
		u.u_r.r_val1 = pp->p_pgrp;
		return;
	}

	case CSIG:
		cp = xcp(gp, vec.m_arg[0]);
		if (cp==NULL) {
			u.u_error = ENXIO;
			return;
		}
		signal(cp->c_pgrp, vec.m_arg[2]);
		u.u_r.r_val1 = vec.m_arg[2];
		return;

	case DEBUG:
		i = vec.m_arg[0];
		if (i<0 || i>NDEBUGS)
			return;
		mcdebugs[i] = vec.m_arg[1];
		if (i==ALL)
			for(i=0;i<NDEBUGS;i++)
				mcdebugs[i] = vec.m_arg[1];
		return;

	default:
		u.u_error = ENXIO;
		return;
	}

}
detach(cp)
register struct chan *cp;
{
	register struct group *master,*sub;
	register index;

	if (cp->c_flags&ISGRP) {
		sub = (struct group *)cp;
		master = sub->g_group;	index = sub->g_index;
		closef(sub->g_file);
		master->g_chans[index] = NULL;
		return;
	} else if (cp->c_flags&PORT && cp->c_ttyp != NULL) {
		closef(cp->c_fy);
		chdrain(cp);
		chfree(cp);
		return;
	}
	if (cp->c_fy && (cp->c_flags&WCLOSE)==0) {
		cp->c_flags |= WCLOSE;
		chwake(cp);
	} else {
		chdrain(cp);
		chfree(cp);
	}
}


mxfalloc(fp)
register struct file *fp;
{
register i;

	if (fp==NULL) {
		u.u_error = ENXIO;
		return(-1);
	}
	i = ufalloc();
	if (i < 0)
		return(i);
	u.u_ofile[i] = fp;
	fp->f_count++;
	u.u_r.r_val1 = i;
	return(i);
}

/*
 * Grow a branch on a tree.
 */

mtree(sub,master)
register struct group *sub, *master;
{
	register i;
	int mtresiz, stresiz;

	if ((mtresiz=mup(master,sub)) == NULL) {
		u.u_error = ENXIO;
		return(NULL);
	}
	if ((stresiz=mdown(sub,master)) <= 0) {
		u.u_error = ENXIO;
		return(NULL);
	}
	if (sub->g_group != NULL) {
		u.u_error = ENXIO;
		return(NULL);
	}
	if (stresiz+mtresiz > NLEVELS) {
		u.u_error = ENXIO;
		return(NULL);
	}
	for (i=0;i<NINDEX;i++) {
		if (master->g_chans[i] != NULL)
			continue;
		master->g_chans[i] = (struct chan *)sub;
		sub->g_group = master;
		sub->g_index = i;
		return(1);
	}
	u.u_error = ENXIO;
	return(NULL);
}

mup(master,sub)
struct group *master, *sub;
{
	register struct group *top;
	register int depth;

	depth = 1;  top = master;
	while (top->g_group) {
		depth++;
		top = top->g_group;
	}
	if(top == sub)
		return(NULL);
	return(depth);
}


mdown(sub,master)
struct group *sub, *master;
{
	register int maxdepth, i, depth;

	if(sub == (struct group *)NULL || (sub->g_state&ISGRP) == 0)
		return(0);
	if(sub == master)
		return(-1);
	maxdepth = 0;
	for(i=0; i<NINDEX; i++) {
		if((depth=mdown(sub->g_chans[i],master)) == -1)
			return(-1);
		maxdepth = (depth>maxdepth) ? depth: maxdepth;
	}
	return(maxdepth+1);
}
