#include "../h/param.h"
#include "../h/systm.h"
#include "../h/uba.h"
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
#include "../h/page.h"

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
	int na, ne, bno, ucp, ap, c;
	struct inode *ip;

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
	if ((bno = malloc(swapmap,(NCARGS+BSIZE-1)/BSIZE)) == 0)
		panic("Out of swap");
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
				bp = getblk(swapdev, swplo+bno+(nc>>BSHIFT));
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
	if (getxfile(ip, nc) || u.u_error)
		goto bad;

	/*
	 * copy back arglist
	 */

	ucp = USRSTACK - nc - NBPW;
	ap = ucp - na*NBPW - 3*NBPW;
	u.u_ar0[SP] = ap;
	suword((caddr_t)ap, na-ne);
	nc = 0;
	for (;;) {
		ap += NBPW;
		if (na==ne) {
			suword((caddr_t)ap, 0);
			ap += NBPW;
		}
		if (--na < 0)
			break;
		suword((caddr_t)ap, ucp);
		do {
			if ((nc&BMASK) == 0) {
				if (bp)
					brelse(bp);
				bp = bread(swapdev, swplo+bno+(nc>>BSHIFT));
				cp = bp->b_un.b_addr;
			}
			subyte((caddr_t)ucp++, (c = *cp++));
			nc++;
		} while(c&0377);
	}
	suword((caddr_t)ap, 0);
	suword((caddr_t)ucp, 0);
	setregs();
bad:
	if (bp)
		brelse(bp);
	if(bno)
		mfree(swapmap, (NCARGS+BSIZE-1)/BSIZE, bno);
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
	register unsigned ds;
	register sep;
	register struct pt_entry *ptaddr;
	register unsigned ts, ss;
	register i, overlay;

	/*
	 * read in first few bytes
	 * of file for segment
	 * sizes:
	 * ux_mag = 407/410/411/405
	 *  407 is plain executable
	 *  410 is RO text
	 *  411 is separated ID
	 *  405 is overlaid text
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
	if(u.u_exdata.ux_mag == 0407) {
		u.u_exdata.ux_dsize += u.u_exdata.ux_tsize;
		u.u_exdata.ux_tsize = 0;
	} else if (u.u_exdata.ux_mag == 0411)
		sep++;
	else if (u.u_exdata.ux_mag == 0405)
		overlay++;
	else if (u.u_exdata.ux_mag != 0410) {
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

	ts = btoc(u.u_exdata.ux_tsize);
	ds = btoc(u.u_exdata.ux_dsize + u.u_exdata.ux_bsize);
	ss = SSIZE + btoc(nargc);
	if (overlay) {
		if (u.u_sep==0 && ctos(ts) != ctos(u.u_tsize) || nargc) {
			u.u_error = ENOMEM;
			goto bad;
		}
		ds = u.u_dsize;
		ss = u.u_ssize;
		sep = u.u_sep;
		xfree();
		xalloc(ip);
		u.u_ar0[PC] = u.u_exdata.ux_entloc + 2; /* skip over entry mask */
	} else {
		if(chksize(ts, ds, ss))
			goto bad;
	
		/*
		 * allocate and clear core
		 * at this point, committed
		 * to the new image
		 */
	
		u.u_prof.pr_scale = 0;
		xfree();
		memfree(mfpr(P0BR) + 4*u.u_tsize,u.u_dsize);	/* free old data area */
		memfree(mfpr(P1BR) + 4*mfpr(P1LR),u.u_ssize);	/* free stack */
		u.u_procp->p_size -= u.u_dsize + u.u_ssize;
		u.u_dsize = 0;
		u.u_ssize = ss;
		mtpr(P0LR,ts);
		u.u_pcb.pcb_p0lr = ts | 0x04000000;	/* also set ast level */
		mtpr(P1LR,0x200000);
		u.u_pcb.pcb_p1lr = 0x200000;
		u.u_procp->p_tsize = ts;	/* save prospective text size for swapin */
		u.u_tsize = ts;
		expand(ss,P1BR);
		u.u_dsize = ds;
		expand(ds,P0BR);
/*		mtpr(TBIA,0); */
		ptaddr = (struct pt_entry *)mfpr(P0BR);
		ptaddr += ts + (u.u_exdata.ux_dsize)/512;
		i = ds - (u.u_exdata.ux_dsize/512);
		while (--i >= 0) {
			clearseg(ptaddr->pg_pfnum);	/* clear bss segment */
			ptaddr++;
		}
		ptaddr = (struct pt_entry *)(mfpr(P1BR) + 4*mfpr(P1LR));
		for(i=ss; --i>=0; ) {
			clearseg(ptaddr->pg_pfnum);	/* clear stack */
			ptaddr++;
		}
		xalloc(ip);
		mtpr(TBIA,1);
	
		/*
		 * read in data segment
		 */
	
		chgprot(0,0,RO);	/* protect the stack */
		u.u_base = (char *)ctob(ts);
		u.u_offset = sizeof(u.u_exdata)+u.u_exdata.ux_tsize;
		u.u_count = u.u_exdata.ux_dsize;
		readi(ip);
		chgprot(0,0,RW);	/* unprotect the stack */
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
	register char *cp;
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
			u.u_pofile[i] &= ~EXCLOSE;
		}
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
	extern int Umap[];
	int ftable[UPAGES + MAXUMEM/128];

	p = u.u_procp;
	p->p_flag &= ~(STRC|SULOCK);
	p->p_clktim = 0;
	for(i=0; i<NSIG; i++)
		u.u_signal[i] = 1;
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
	xfree();
	acct();

	/* free data, stack, u-area, and page-tables */
	memfree((int *)&u + UPAGES*128 + u.u_tsize, u.u_dsize);
	memfree(((int *)&u) + USIZE*128 - u.u_ssize, u.u_ssize);
	for(i=USIZE; --i>=0; )
		ftable[i] = Umap[i];
	memfree(ftable, USIZE);

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
	register struct proc *p1, *p2;
	register a;

	/*
	 * Make sure there's enough swap space for max
	 * core image, thus reducing chances of running out
	 */
	if ((a = malloc(swapmap, ctod(MAXMEM))) == 0) {
		u.u_error = ENOMEM;
		goto out;
	}
	mfree(swapmap, ctod(MAXMEM), a);
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
		goto out;
	}
	p1 = u.u_procp;
	if(newproc()) {
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
	register  n, d;
	register struct pt_entry *ptaddr;

	/*
	 * set n to new data size
	 * set d to new-old
	 */

	n = btoc(((struct a *)u.u_ap)->nsiz);
	if(!u.u_sep)
		n -= ctos(u.u_tsize) * stoc(1);
	if(n < 0)
		n = 0;
	d = n - u.u_dsize;
	if(chksize(u.u_tsize, u.u_dsize+d, u.u_ssize))
		return;
	u.u_dsize += d;
	expand(d,P0BR);
	if (d>0) {	/* bigger */
		ptaddr = (struct pt_entry *)mfpr(P0BR);
		ptaddr += u.u_tsize + u.u_dsize - d;	/* point at first new page */
		while(d--)
			clearseg(ptaddr++->pg_pfnum);
	}
}
