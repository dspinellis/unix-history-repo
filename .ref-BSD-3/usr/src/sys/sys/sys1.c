/*	sys1.c	2.2	2/10/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/mtpr.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/seg.h"
#include "../h/acct.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/text.h"

/*
 * exec system call, with and without environments.
 */
struct execa {
	char	*fname;
	char	**argp;
	char	**envp;
};

exec()
{
	((struct execa *)u.u_ap)->envp = NULL;
	exece();
}

exece()
{
	register nc;
	register char *cp;
	register struct buf *bp;
	register struct execa *uap;
	int na, ne, ucp, ap, c;
	struct inode *ip;
	swblk_t bno;

	if ((ip = namei(uchar, 0)) == NULL)
		return;
	bno = 0;
	bp = 0;
	if(access(ip, IEXEC))
		goto bad;
	if((ip->i_mode & IFMT) != IFREG ||
	   (ip->i_mode & (IEXEC|(IEXEC>>3)|(IEXEC>>6))) == 0) {
		u.u_error = EACCES;
		goto bad;
	}
	/*
	 * Collect arguments on "file" in swap space.
	 */
	na = 0;
	ne = 0;
	nc = 0;
	uap = (struct execa *)u.u_ap;
	if ((bno = malloc(swapmap, ctod(clrnd((int) btoc(NCARGS))))) == 0) {
		swkill(u.u_procp);
		goto bad;
	}
	if (bno % CLSIZE)
		panic("execa malloc");
	if (uap->argp) for (;;) {
		ap = NULL;
		if (uap->argp) {
			ap = fuword((caddr_t)uap->argp);
			uap->argp++;
		}
		if (ap==NULL && uap->envp) {
			uap->argp = NULL;
			if ((ap = fuword((caddr_t)uap->envp)) == NULL)
				break;
			uap->envp++;
			ne++;
		}
		if (ap==NULL)
			break;
		na++;
		if(ap == -1)
			u.u_error = EFAULT;
		do {
			if (nc >= NCARGS-1)
				u.u_error = E2BIG;
			if ((c = fubyte((caddr_t)ap++)) < 0)
				u.u_error = EFAULT;
			if (u.u_error)
				goto bad;
			if ((nc&BMASK) == 0) {
				if (bp)
					bawrite(bp);
				bp = getblk(swapdev, (daddr_t)(dbtofsb(swplo+bno)+(nc>>BSHIFT)));
				cp = bp->b_un.b_addr;
			}
			nc++;
			*cp++ = c;
		} while (c>0);
	}
	if (bp)
		bawrite(bp);
	bp = 0;
	nc = (nc + NBPW-1) & ~(NBPW-1);
	if (getxfile(ip, nc) || u.u_error) {
		for (c = 0; c < nc; c += BSIZE)
			if (bp = baddr(swapdev, dbtofsb(swplo+bno)+(c>>BSHIFT))) {
				bp->b_flags &= ~B_DELWRI;
				brelse(bp);
				bp = 0;
			}
		goto bad;
	}

	/*
	 * copy back arglist
	 */

	ucp = USRSTACK - nc - NBPW;
	ap = ucp - na*NBPW - 3*NBPW;
	u.u_ar0[SP] = ap;
	VOID suword((caddr_t)ap, na-ne);
	nc = 0;
	for (;;) {
		ap += NBPW;
		if (na==ne) {
			VOID suword((caddr_t)ap, 0);
			ap += NBPW;
		}
		if (--na < 0)
			break;
		VOID suword((caddr_t)ap, ucp);
		do {
			if ((nc&BMASK) == 0) {
				if (bp)
					brelse(bp);
				bp = bread(swapdev, (daddr_t)(dbtofsb(swplo+bno)+(nc>>BSHIFT)));
				bp->b_flags &= ~B_DELWRI;
				cp = bp->b_un.b_addr;
			}
			VOID subyte((caddr_t)ucp++, (c = *cp++));
			nc++;
		} while(c&0377);
	}
	VOID suword((caddr_t)ap, 0);
	VOID suword((caddr_t)ucp, 0);
	setregs();
bad:
	if (bp)
		brelse(bp);
	if (bno)
		mfree(swapmap, ctod(clrnd((int) btoc(NCARGS))), bno);
	iput(ip);
}

/*
 * Read in and set up memory for executed file.
 * Zero return is normal;
 * non-zero means only the text is being replaced
 */
getxfile(ip, nargc)
register struct inode *ip;
{
	register sep;
	register size_t ts, ds, ss;
	register int overlay;
	int pagi = 0;

	/*
	 * read in first few bytes
	 * of file for segment
	 * sizes:
	 * ux_mag = 407/410/411/405
	 *  407 is plain executable
	 *  410 is RO text
	 *  411 is separated ID
	 *  405 is overlaid text
	 *  412 is demand paged plain executable (NOT IMPLEMENTED)
	 *  413 is demand paged RO text
	 */

	u.u_base = (caddr_t)&u.u_exdata;
	u.u_count = sizeof(u.u_exdata);
	u.u_offset = 0;
	u.u_segflg = 1;
	readi(ip);
	u.u_segflg = 0;
	if(u.u_error)
		goto bad;
	if (u.u_count!=0) {
		u.u_error = ENOEXEC;
		goto bad;
	}
	sep = 0;
	overlay = 0;
	switch (u.u_exdata.ux_mag) {

	case 0405:
		overlay++;
		break;

	case 0412:
		u.u_error = ENOEXEC;
		goto bad;

	case 0407:
		u.u_exdata.ux_dsize += u.u_exdata.ux_tsize;
		u.u_exdata.ux_tsize = 0;
		break;

	case 0413:
		pagi = SPAGI;
		/* fall into ... */

	case 0410:
		if (u.u_exdata.ux_tsize == 0) {
			u.u_error = ENOEXEC;
			goto bad;
		}
		break;

	case 0411:
		u.u_error = ENOEXEC;
		goto bad;

	default:
		u.u_error = ENOEXEC;
		goto bad;
	}
	if(u.u_exdata.ux_tsize!=0 && (ip->i_flag&ITEXT)==0 && ip->i_count!=1) {
		u.u_error = ETXTBSY;
		goto bad;
	}

	/*
	 * find text and data sizes
	 * try them out for possible
	 * exceed of max sizes
	 */

	ts = clrnd(btoc(u.u_exdata.ux_tsize));
	ds = clrnd(btoc((u.u_exdata.ux_dsize+u.u_exdata.ux_bsize)));
	ss = clrnd(SSIZE + btoc(nargc));
	if (overlay) {
		if ((u.u_procp->p_flag & SPAGI) || u.u_sep==0 && ctos(ts) != ctos(u.u_tsize) || nargc) {
			u.u_error = ENOMEM;
			goto bad;
		}
		ds = u.u_dsize;
		ss = u.u_ssize;
		sep = u.u_sep;
		xfree();
		xalloc(ip, pagi);
		u.u_ar0[PC] = u.u_exdata.ux_entloc + 2; /* skip over entry mask */
	} else {
		if (chksize(ts, ds, ss))
			goto bad;
		u.u_cdmap = zdmap;
		u.u_csmap = zdmap;
		if (swpexpand(ds, ss, &u.u_cdmap, &u.u_csmap) == NULL)
			goto bad;

		/*
		 * At this point, committed to the new image!
		 * Release virtual memory resources of old process, and
		 * initialize the virtual memory of the new process.
		 * If we resulted from vfork(), instead wakeup our
		 * parent who will set SVFDONE when he has taken back
		 * our resources.
		 */
		u.u_prof.pr_scale = 0;
		if ((u.u_procp->p_flag & SVFORK) == 0)
			vrelvm();
		else {
			u.u_procp->p_flag &= ~SVFORK;
			u.u_procp->p_flag |= SKEEP;
			wakeup((caddr_t)u.u_procp);
			while ((u.u_procp->p_flag & SVFDONE) == 0)
				sleep((caddr_t)u.u_procp, PZERO - 1);
			u.u_procp->p_flag &= ~(SVFDONE|SKEEP);
		}
		u.u_procp->p_flag &= ~SPAGI;
		u.u_procp->p_flag |= pagi;
		u.u_dmap = u.u_cdmap;
		u.u_smap = u.u_csmap;
		vgetvm(ts, ds, ss);

		if (pagi == 0) {
			/*
			 * Read in data segment.
			 */
			u.u_base = (char *)ctob(ts);
			u.u_offset = sizeof(u.u_exdata)+u.u_exdata.ux_tsize;
			u.u_count = u.u_exdata.ux_dsize;
			readi(ip);
		}
		xalloc(ip, pagi);
		if (pagi && u.u_procp->p_textp)
			vinifod((struct fpte *)dptopte(u.u_procp, 0),
			    PG_FTEXT, u.u_procp->p_textp->x_iptr,
			    1 + ts/CLSIZE, (int)btoc(u.u_exdata.ux_dsize));

		/* THIS SHOULD BE DONE AT A LOWER LEVEL, IF AT ALL */
		mtpr(TBIA,1);

		/*
		 * set SUID/SGID protections, if no tracing
		 */
		if ((u.u_procp->p_flag&STRC)==0) {
			if(ip->i_mode&ISUID)
				if(u.u_uid != 0) {
					u.u_uid = ip->i_uid;
					u.u_procp->p_uid = ip->i_uid;
				}
			if(ip->i_mode&ISGID)
				u.u_gid = ip->i_gid;
		} else
			psignal(u.u_procp, SIGTRC);
	}
	u.u_tsize = ts;
	u.u_dsize = ds;
	u.u_ssize = ss;
	u.u_sep = sep;
bad:
	return(overlay);
}

/*
 * Clear registers on exec
 */
setregs()
{
	register int *rp;
	register i;

	for(rp = &u.u_signal[0]; rp < &u.u_signal[NSIG]; rp++)
		if((*rp & 1) == 0)
			*rp = 0;
/*
	for(rp = &u.u_ar0[0]; rp < &u.u_ar0[16];)
		*rp++ = 0;
*/
	u.u_ar0[PC] = u.u_exdata.ux_entloc + 2; /* skip over entry mask */
	for(i=0; i<NOFILE; i++) {
		if (u.u_pofile[i]&EXCLOSE) {
			closef(u.u_ofile[i]);
			u.u_ofile[i] = NULL;
		}
		u.u_pofile[i] &= ~EXCLOSE;
	}
	/*
	 * Remember file name for accounting.
	 */
	u.u_acflag &= ~AFORK;
	bcopy((caddr_t)u.u_dbuf, (caddr_t)u.u_comm, DIRSIZ);
}

/*
 * exit system call:
 * pass back caller's arg
 */
rexit()
{
	register struct a {
		int	rval;
	} *uap;

	uap = (struct a *)u.u_ap;
	exit((uap->rval & 0377) << 8);
}

/*
 * Release resources.
 * Save u. area for parent to look at.
 * Enter zombie state.
 * Wake up parent and init processes,
 * and dispose of children.
 */
exit(rv)
{
	register int i;
	register struct proc *p, *q;
	register struct file *f;

#ifdef ERNIE
	vmsizmon();
#endif
	p = u.u_procp;
	p->p_flag &= ~(STRC|SULOCK);
	p->p_flag |= SWEXIT;
	p->p_clktim = 0;
	rate.v_pgin -= p->p_aveflt;
	p->p_aveflt = 0;
	for(i=0; i<NSIG; i++)
		u.u_signal[i] = 1;
	/*
	 * Release virtual memory.  If we resulted from
	 * a vfork(), instead give the resources back to
	 * the parent.
	 */
	if ((p->p_flag & SVFORK) == 0) {
		vrelvm();
		vrelpt(u.u_procp);
	} else {
		p->p_flag &= ~SVFORK;
		wakeup((caddr_t)p);
		while ((p->p_flag & SVFDONE) == 0)
			sleep((caddr_t)p, PZERO - 1);
		p->p_flag &= ~SVFDONE;
	}
	for(i=0; i<NOFILE; i++) {
		f = u.u_ofile[i];
		u.u_ofile[i] = NULL;
		closef(f);
	}
	plock(u.u_cdir);
	iput(u.u_cdir);
	if (u.u_rdir) {
		plock(u.u_rdir);
		iput(u.u_rdir);
	}
	acct();

	vrelu(u.u_procp, 0);

	multprog--;
	p->p_stat = SZOMB;
	((struct xproc *)p)->xp_xstat = rv;
	((struct xproc *)p)->xp_utime = u.u_cutime + u.u_utime;
	((struct xproc *)p)->xp_stime = u.u_cstime + u.u_stime;
	for(q = &proc[0]; q < &proc[NPROC]; q++)
		if(q->p_ppid == p->p_pid) {
			wakeup((caddr_t)&proc[1]);
			q->p_ppid = 1;
			if (q->p_stat==SSTOP)
				setrun(q);
		}
	for(q = &proc[0]; q < &proc[NPROC]; q++)
		if(p->p_ppid == q->p_pid) {
			wakeup((caddr_t)q);
			swtch();
			/* no return */
		}
	swtch();
}

/*
 * Wait system call.
 * Search for a terminated (zombie) child,
 * finally lay it to rest, and collect its status.
 * Look also for stopped (traced) children,
 * and pass back status from them.
 */
wait()
{
	register f;
	register struct proc *p;

	f = 0;

loop:
	for(p = &proc[0]; p < &proc[NPROC]; p++)
	if(p->p_ppid == u.u_procp->p_pid) {
		f++;
		if(p->p_stat == SZOMB) {
			u.u_r.r_val1 = p->p_pid;
			u.u_r.r_val2 = ((struct xproc *)p)->xp_xstat;
			u.u_cutime += ((struct xproc *)p)->xp_utime;
			u.u_cstime += ((struct xproc *)p)->xp_stime;
			((struct xproc *)p)->xp_xstat = 0;
			((struct xproc *)p)->xp_utime = 0;
			((struct xproc *)p)->xp_stime = 0;
			p->p_stat = NULL;
			p->p_pid = 0;
			p->p_ppid = 0;
			p->p_sig = 0;
			p->p_pgrp = 0;
			p->p_flag = 0;
			p->p_wchan = 0;
			return;
		}
		if(p->p_stat == SSTOP) {
			if((p->p_flag&SWTED) == 0) {
				p->p_flag |= SWTED;
				u.u_r.r_val1 = p->p_pid;
				u.u_r.r_val2 = (fsig(p)<<8) | 0177;
				return;
			}
			continue;
		}
	}
	if(f) {
		sleep((caddr_t)u.u_procp, PWAIT);
		goto loop;
	}
	u.u_error = ECHILD;
}

/*
 * fork system call.
 */
fork()
{

	if (swpexpand(u.u_dsize, u.u_ssize, &u.u_dmap, &u.u_smap) == 0) {
		swkill(u.u_procp);
		return;
	}
	u.u_cdmap = zdmap;
	u.u_csmap = zdmap;
	if (swpexpand(u.u_dsize, u.u_ssize, &u.u_cdmap, &u.u_csmap) == 0) {
		u.u_error = ENOMEM;
		u.u_r.r_val2 = 0;
		return;
	}
	fork1(0);
}

fork1(isvfork)
{
	register struct proc *p1, *p2;
	register a;

	a = 0;
	p2 = NULL;
	for(p1 = &proc[0]; p1 < &proc[NPROC]; p1++) {
		if (p1->p_stat==NULL && p2==NULL)
			p2 = p1;
		else {
			if (p1->p_uid==u.u_uid && p1->p_stat!=NULL)
				a++;
		}
	}
	/*
	 * Disallow if
	 *  No processes at all;
	 *  not su and too many procs owned; or
	 *  not su and would take last slot.
	 */
	if (p2==NULL || (u.u_uid!=0 && (p2==&proc[NPROC-1] || a>MAXUPRC))) {
		u.u_error = EAGAIN;
		if (!isvfork) {
			VOID vsexpand(0, &u.u_cdmap);
			VOID vsexpand(0, &u.u_csmap);
		}
		goto out;
	}
	p1 = u.u_procp;
	if(newproc(isvfork)) {
		u.u_r.r_val1 = p1->p_pid;
		u.u_r.r_val2 = 1;  /* child */
		u.u_start = time;
		u.u_cstime = 0;
		u.u_stime = 0;
		u.u_cutime = 0;
		u.u_utime = 0;
		u.u_acflag = AFORK;
		return;
	}
	u.u_r.r_val1 = p2->p_pid;

out:
	u.u_r.r_val2 = 0;
}

/*
 * break system call.
 *  -- bad planning: "break" is a dirty word in C.
 */
sbreak()
{
	struct a {
		char	*nsiz;
	};
	register int n, d;

	/*
	 * set n to new data size
	 * set d to new-old
	 */

	n = btoc(((struct a *)u.u_ap)->nsiz);
	if (!u.u_sep)
		n -= ctos(u.u_tsize) * stoc(1);
	if (n < 0)
		n = 0;
	d = clrnd(n - u.u_dsize);
	if (chksize(u.u_tsize, u.u_dsize+d, u.u_ssize))
		return;
	expand(d, P0BR);
}
