/*	kern_proc.c	4.12	81/04/28	*/

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
#include "/usr/include/wait.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/text.h"
#include "../h/psl.h"
#include "../h/vlimit.h"
#include "../h/file.h"

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
	int indir, uid, gid;
	char *sharg;
	struct inode *ip;
	swblk_t bno;
	char cfname[DIRSIZ];
	char cfarg[SHSIZE];

	if ((ip = namei(uchar, 0)) == NULL)
		return;

	bno = 0;
	bp = 0;
	indir = 0;
	uid = u.u_uid;
	gid = u.u_gid;

	if (ip->i_mode & ISUID)
		uid = ip->i_uid;
	if (ip->i_mode & ISGID)
		gid = ip->i_gid;

  again:
	if(access(ip, IEXEC))
		goto bad;
	if((u.u_procp->p_flag&STRC) && access(ip, IREAD))
		goto bad;
	if((ip->i_mode & IFMT) != IFREG ||
	   (ip->i_mode & (IEXEC|(IEXEC>>3)|(IEXEC>>6))) == 0) {
		u.u_error = EACCES;
		goto bad;
	}

	/*
	 * Read in first few bytes of file for segment sizes, ux_mag:
	 *	407 = plain executable
	 *	410 = RO text
	 *	413 = demand paged RO text
	 * Also an ASCII line beginning with #! is
	 * the file name of a ``shell'' and arguments may be prepended
	 * to the argument list if given here.
	 *
	 * SHELL NAMES ARE LIMITED IN LENGTH.
	 *
	 * ONLY ONE ARGUMENT MAY BE PASSED TO THE SHELL FROM
	 * THE ASCII LINE.
	 */
	u.u_base = (caddr_t)&u.u_exdata;
	u.u_count = sizeof(u.u_exdata);
	u.u_offset = 0;
	u.u_segflg = 1;
	readi(ip);
	u.u_segflg = 0;
	if(u.u_error)
		goto bad;
	if (u.u_count > sizeof(u.u_exdata) - sizeof(u.u_exdata.Ux_A)
		&& u.u_exdata.ux_shell[0] != '#') {
		u.u_error = ENOEXEC;
		goto bad;
	}
	switch (u.u_exdata.ux_mag) {

	case 0407:
		u.u_exdata.ux_dsize += u.u_exdata.ux_tsize;
		u.u_exdata.ux_tsize = 0;
		break;

	case 0413:
	case 0410:
		if (u.u_exdata.ux_tsize == 0) {
			u.u_error = ENOEXEC;
			goto bad;
		}
		break;

	default:
		if (u.u_exdata.ux_shell[0] != '#' ||
		    u.u_exdata.ux_shell[1] != '!' ||
		    indir) {
			u.u_error = ENOEXEC;
			goto bad;
		}
		cp = &u.u_exdata.ux_shell[2];		/* skip "#!" */
		while (cp < &u.u_exdata.ux_shell[SHSIZE]) {
			if (*cp == '\t')
				*cp = ' ';
			else if (*cp == '\n') {
				*cp = '\0';
				break;
			}
			cp++;
		}
		if (*cp != '\0') {
			u.u_error = ENOEXEC;
			goto bad;
		}
		cp = &u.u_exdata.ux_shell[2];
		while (*cp == ' ')
			cp++;
		u.u_dirp = cp;
		while (*cp && *cp != ' ')
			cp++;
		sharg = NULL;
		if (*cp) {
			*cp++ = '\0';
			while (*cp == ' ')
				cp++;
			if (*cp) {
				bcopy((caddr_t)cp, (caddr_t)cfarg, SHSIZE);
				sharg = cfarg;
			}
		}
		bcopy((caddr_t)u.u_dbuf, (caddr_t)cfname, DIRSIZ);
		indir = 1;
		iput(ip);
		ip = namei(schar, 0);
		if (ip == NULL)
			return;
		goto again;
	}

	/*
	 * Collect arguments on "file" in swap space.
	 */
	na = 0;
	ne = 0;
	nc = 0;
	uap = (struct execa *)u.u_ap;
	if ((bno = rmalloc(argmap, ctod(clrnd((int) btoc(NCARGS))))) == 0) {
		swkill(u.u_procp, "exece");
		goto bad;
	}
	if (bno % CLSIZE)
		panic("execa rmalloc");
	if (uap->argp) for (;;) {
		ap = NULL;
		if (indir && (na == 1 || na == 2 && sharg))
			ap = (int)uap->fname;
		else if (uap->argp) {
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
			if (indir && na == 2 && sharg != NULL)
				c = *sharg++ & 0377;
			else if ((c = fubyte((caddr_t)ap++)) < 0)
				u.u_error = EFAULT;
			if (u.u_error) {
				if (bp)
					brelse(bp);
				bp = 0;
				goto badarg;
			}
			if ((nc&BMASK) == 0) {
				if (bp)
					bdwrite(bp);
				bp = getblk(argdev,
				    (daddr_t)(dbtofsb(bno)+(nc>>BSHIFT)));
				cp = bp->b_un.b_addr;
			}
			nc++;
			*cp++ = c;
		} while (c>0);
	}
	if (bp)
		bdwrite(bp);
	bp = 0;
	nc = (nc + NBPW-1) & ~(NBPW-1);
	if (indir)
		bcopy((caddr_t)cfname, (caddr_t)u.u_dbuf, DIRSIZ);
	getxfile(ip, nc + (na+4)*NBPW, uid, gid);
	if (u.u_error) {
badarg:
		for (c = 0; c < nc; c += BSIZE)
			if (bp = baddr(argdev, dbtofsb(bno)+(c>>BSHIFT))) {
				bp->b_flags |= B_AGE;		/* throw away */
				bp->b_flags &= ~B_DELWRI;	/* cancel io */
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
	(void) suword((caddr_t)ap, na-ne);
	nc = 0;
	for (;;) {
		ap += NBPW;
		if (na==ne) {
			(void) suword((caddr_t)ap, 0);
			ap += NBPW;
		}
		if (--na < 0)
			break;
		(void) suword((caddr_t)ap, ucp);
		do {
			if ((nc&BMASK) == 0) {
				if (bp)
					brelse(bp);
				bp = bread(argdev,
				    (daddr_t)(dbtofsb(bno)+(nc>>BSHIFT)));
				bp->b_flags |= B_AGE;		/* throw away */
				bp->b_flags &= ~B_DELWRI;	/* cancel io */
				cp = bp->b_un.b_addr;
			}
			(void) subyte((caddr_t)ucp++, (c = *cp++));
			nc++;
		} while(c&0377);
	}
	(void) suword((caddr_t)ap, 0);
	(void) suword((caddr_t)ucp, 0);
	setregs();
bad:
	if (bp)
		brelse(bp);
	if (bno)
		rmfree(argmap, ctod(clrnd((int) btoc(NCARGS))), bno);
	iput(ip);
}

/*
 * Read in and set up memory for executed file.
 */
getxfile(ip, nargc, uid, gid)
register struct inode *ip;
{
	register size_t ts, ds, ss;
	int pagi;

	if (u.u_exdata.ux_mag == 0413)
		pagi = SPAGI;
	else
		pagi = 0;

	if(u.u_exdata.ux_tsize!=0 && (ip->i_flag&ITEXT)==0 && ip->i_count!=1) {
		register struct file *fp;

		for (fp = file; fp < fileNFILE; fp++)
			if (fp->f_inode == ip && (fp->f_flag&FWRITE)) {
				u.u_error = ETXTBSY;
				goto bad;
			}
	}

	/*
	 * find text and data sizes
	 * try them out for possible
	 * exceed of max sizes
	 */

	ts = clrnd(btoc(u.u_exdata.ux_tsize));
	ds = clrnd(btoc((u.u_exdata.ux_dsize+u.u_exdata.ux_bsize)));
	ss = clrnd(SSIZE + btoc(nargc));
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
	u.u_procp->p_flag &= ~(SPAGI|SSEQL|SUANOM|SNUSIG);
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
	mtpr(TBIA, 0);

	/*
	 * set SUID/SGID protections, if no tracing
	 */
	if ((u.u_procp->p_flag&STRC)==0) {
#ifndef	MELB
		if(u.u_uid != 0)
#endif
		{
			u.u_uid = uid;
			u.u_procp->p_uid = uid;
		}
		u.u_gid = gid;
	} else
		psignal(u.u_procp, SIGTRAP);
	u.u_tsize = ts;
	u.u_dsize = ds;
	u.u_ssize = ss;
bad:
	return;
}

/*
 * Clear registers on exec
 */
setregs()
{
	register int (**rp)();
	register i;
	long sigmask;

	for(rp = &u.u_signal[0], sigmask = 1L; rp < &u.u_signal[NSIG];
	    sigmask <<= 1, rp++) {
		switch (*rp) {

		case SIG_IGN:
		case SIG_DFL:
		case SIG_HOLD:
			continue;

		default:
			/*
			 * Normal or deferring catch; revert to default.
			 */
			(void) spl6();
			*rp = SIG_DFL;
			if ((int)*rp & 1)
				u.u_procp->p_siga0 |= sigmask;
			else
				u.u_procp->p_siga1 &= ~sigmask;
			if ((int)*rp & 2)
				u.u_procp->p_siga1 |= sigmask;
			else
				u.u_procp->p_siga1 &= ~sigmask;
			(void) spl0();
			continue;
		}
	}
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
	register int x;

#ifdef PGINPROF
	vmsizmon();
#endif
	p = u.u_procp;
	p->p_flag &= ~(STRC|SULOCK);
	p->p_flag |= SWEXIT;
	p->p_clktim = 0;
	(void) spl6();
	if ((int)SIG_IGN & 1)
		p->p_siga0 = ~0;
	else
		p->p_siga0 = 0;
	if ((int)SIG_IGN & 2)
		p->p_siga1 = ~0;
	else
		p->p_siga1 = 0;
	(void) spl0();
	p->p_cpticks = 0;
	p->p_pctcpu = 0;
	for(i=0; i<NSIG; i++)
		u.u_signal[i] = SIG_IGN;
	/*
	 * Release virtual memory.  If we resulted from
	 * a vfork(), instead give the resources back to
	 * the parent.
	 */
	if ((p->p_flag & SVFORK) == 0)
		vrelvm();
	else {
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
	u.u_limit[LIM_FSIZE] = INFINITY;
	acct();
	vrelpt(u.u_procp);
	vrelu(u.u_procp, 0);
	multprog--;
/*	spl7();			/* clock will get mad because of overlaying */
	p->p_stat = SZOMB;
	noproc = 1;
	i = PIDHASH(p->p_pid);
	x = p - proc;
	if (pidhash[i] == x)
		pidhash[i] = p->p_idhash;
	else {
		for (i = pidhash[i]; i != 0; i = proc[i].p_idhash)
			if (proc[i].p_idhash == x) {
				proc[i].p_idhash = p->p_idhash;
				goto done;
			}
		panic("exit");
	}
	if (p->p_pid == 1)
		panic("init died");
done:
	((struct xproc *)p)->xp_xstat = rv;		/* overlay */
	((struct xproc *)p)->xp_vm = u.u_vm;		/* overlay */
	vmsadd(&((struct xproc *)p)->xp_vm, &u.u_cvm);
	for(q = proc; q < procNPROC; q++)
		if(q->p_pptr == p) {
			q->p_pptr = &proc[1];
			q->p_ppid = 1;
			wakeup((caddr_t)&proc[1]);
			/*
			 * Traced processes are killed
			 * since their existence means someone is screwing up.
			 * Stopped processes are sent a hangup and a continue.
			 * This is designed to be ``safe'' for setuid
			 * processes since they must be willing to tolerate
			 * hangups anyways.
			 */
			if (q->p_flag&STRC) {
				q->p_flag &= ~STRC;
				psignal(q, SIGKILL);
			} else if (q->p_stat == SSTOP) {
				psignal(q, SIGHUP);
				psignal(q, SIGCONT);
			}
			/*
			 * Protect this process from future
			 * tty signals, clear TSTP/TTIN/TTOU if pending,
			 * and set SDETACH bit on procs.
			 */
			(void) spgrp(q, -1);
		}
	wakeup((caddr_t)p->p_pptr);
	psignal(p->p_pptr, SIGCHLD);
	swtch();
}

wait()
{
	struct vtimes vm;
	struct vtimes *vp;

	if ((u.u_ar0[PS] & PSL_ALLCC) != PSL_ALLCC) {
		wait1(0, (struct vtimes *)0);
		return;
	}
	vp = (struct vtimes *)u.u_ar0[R1];
	wait1(u.u_ar0[R0], &vm);
	if (u.u_error)
		return;
	(void) copyout((caddr_t)&vm, (caddr_t)vp, sizeof (struct vtimes));
}

/*
 * Wait system call.
 * Search for a terminated (zombie) child,
 * finally lay it to rest, and collect its status.
 * Look also for stopped (traced) children,
 * and pass back status from them.
 */
wait1(options, vp)
	register options;
	struct vtimes *vp;
{
	register f;
	register struct proc *p;

	f = 0;
loop:
	for(p = proc; p < procNPROC; p++)
	if(p->p_pptr == u.u_procp) {
		f++;
		if(p->p_stat == SZOMB) {
			u.u_r.r_val1 = p->p_pid;
			u.u_r.r_val2 = ((struct xproc *)p)->xp_xstat;
			((struct xproc *)p)->xp_xstat = 0;
			if (vp)
				*vp = ((struct xproc *)p)->xp_vm;
			vmsadd(&u.u_cvm, &((struct xproc *)p)->xp_vm);
			((struct xproc *)p)->xp_vm = zvms;
			p->p_stat = NULL;
			p->p_pid = 0;
			p->p_ppid = 0;
			p->p_pptr = 0;
			p->p_sig = 0;
			p->p_siga0 = 0;
			p->p_siga1 = 0;
			p->p_pgrp = 0;
			p->p_flag = 0;
			p->p_wchan = 0;
			p->p_cursig = 0;
			return;
		}
		if (p->p_stat == SSTOP && (p->p_flag&SWTED)==0 &&
		    (p->p_flag&STRC || options&WUNTRACED)) {
			p->p_flag |= SWTED;
			u.u_r.r_val1 = p->p_pid;
			u.u_r.r_val2 = (p->p_cursig<<8) | WSTOPPED;
			return;
		}
	}
	if (f==0) {
		u.u_error = ECHILD;
		return;
	}
	if (options&WNOHANG) {
		u.u_r.r_val1 = 0;
		return;
	}
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		u.u_eosys = RESTARTSYS;
		return;
	}
	sleep((caddr_t)u.u_procp, PWAIT);
	goto loop;
}

/*
 * fork system call.
 */
fork()
{

	u.u_cdmap = zdmap;
	u.u_csmap = zdmap;
	if (swpexpand(u.u_dsize, u.u_ssize, &u.u_cdmap, &u.u_csmap) == 0) {
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
	for(p1 = proc; p1 < procNPROC; p1++) {
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
	if (p2==NULL)
		tablefull("proc");
	if (p2==NULL || (u.u_uid!=0 && (p2==procNPROC-1 || a>MAXUPRC))) {
		u.u_error = EAGAIN;
		if (!isvfork) {
			(void) vsexpand(0, &u.u_cdmap, 1);
			(void) vsexpand(0, &u.u_csmap, 1);
		}
		goto out;
	}
	p1 = u.u_procp;
	if(newproc(isvfork)) {
		u.u_r.r_val1 = p1->p_pid;
		u.u_r.r_val2 = 1;  /* child */
		u.u_start = time;
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
	if (ctob(u.u_dsize+d) > u.u_limit[LIM_DATA]) {
		u.u_error = ENOMEM;
		return;
	}
	if (chksize(u.u_tsize, u.u_dsize+d, u.u_ssize))
		return;
	if (swpexpand(u.u_dsize+d, u.u_ssize, &u.u_dmap, &u.u_smap)==0)
		return;
	expand(d, P0BR);
}
