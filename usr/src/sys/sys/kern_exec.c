/*	kern_exec.c	6.2	83/08/23	*/

#include "../machine/reg.h"
#include "../machine/pte.h"
#include "../machine/psl.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/inode.h"
#include "../h/seg.h"
#include "../h/vm.h"
#include "../h/text.h"
#include "../h/file.h"
#include "../h/uio.h"
#include "../h/nami.h"
#include "../h/acct.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif

/*
 * exec system call, with and without environments.
 */
struct execa {
	char	*fname;
	char	**argp;
	char	**envp;
};

execv()
{
	((struct execa *)u.u_ap)->envp = NULL;
	execve();
}

execve()
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
	char cfname[MAXCOMLEN + 1];
	char cfarg[SHSIZE];
	int resid;

	if ((ip = namei(uchar, LOOKUP, 1)) == NULL)
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
	if (access(ip, IEXEC))
		goto bad;
	if ((u.u_procp->p_flag&STRC) && access(ip, IREAD))
		goto bad;
	if ((ip->i_mode & IFMT) != IFREG ||
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
	u.u_exdata.ux_shell[0] = 0;	/* for zero length files */
	u.u_error = rdwri(UIO_READ, ip, (caddr_t)&u.u_exdata, sizeof (u.u_exdata),
	    0, 1, &resid);
	if (u.u_error)
		goto bad;
	u.u_count = resid;
#ifndef lint
	if (u.u_count > sizeof(u.u_exdata) - sizeof(u.u_exdata.Ux_A) &&
	    u.u_exdata.ux_shell[0] != '#') {
		u.u_error = ENOEXEC;
		goto bad;
	}
#endif
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
		if (u.u_dent.d_namlen > MAXCOMLEN)
			u.u_dent.d_namlen = MAXCOMLEN;
		bcopy((caddr_t)u.u_dent.d_name, (caddr_t)cfname,
		    (unsigned)(u.u_dent.d_namlen + 1));
		cfname[MAXCOMLEN] = 0;
		indir = 1;
		iput(ip);
		ip = namei(schar, LOOKUP, 1);
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
	if ((bno = rmalloc(argmap, (long)ctod(clrnd((int)btoc(NCARGS))))) == 0) {
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
		if (ap == NULL)
			break;
		na++;
		if (ap == -1)
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
			if (nc % (CLSIZE*NBPG) == 0) {
				if (bp)
					bdwrite(bp);
				bp = getblk(argdev, bno + ctod(nc / NBPG),
				    CLSIZE*NBPG);
				cp = bp->b_un.b_addr;
			}
			nc++;
			*cp++ = c;
		} while (c > 0);
	}
	if (bp)
		bdwrite(bp);
	bp = 0;
	nc = (nc + NBPW-1) & ~(NBPW-1);
	if (indir) {
		u.u_dent.d_namlen = strlen(cfname);
		bcopy((caddr_t)cfname, (caddr_t)u.u_dent.d_name,
		    (unsigned)(u.u_dent.d_namlen + 1));
	}
	getxfile(ip, nc + (na+4)*NBPW, uid, gid);
	if (u.u_error) {
badarg:
		for (c = 0; c < nc; c += CLSIZE*NBPG) {
			bp = baddr(argdev, bno + ctod(c / NBPG), CLSIZE*NBPG);
			if (bp) {
				bp->b_flags |= B_AGE;		/* throw away */
				bp->b_flags &= ~B_DELWRI;	/* cancel io */
				brelse(bp);
				bp = 0;
			}
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
			if (nc % (CLSIZE*NBPG) == 0) {
				if (bp)
					brelse(bp);
				bp = bread(argdev, bno + ctod(nc / NBPG),
				    CLSIZE*NBPG);
				bp->b_flags |= B_AGE;		/* throw away */
				bp->b_flags &= ~B_DELWRI;	/* cancel io */
				cp = bp->b_un.b_addr;
			}
			(void) subyte((caddr_t)ucp++, (c = *cp++));
			nc++;
		} while(c&0377);
	}
	(void) suword((caddr_t)ap, 0);
	setregs();
bad:
	if (bp)
		brelse(bp);
	if (bno)
		rmfree(argmap, (long)ctod(clrnd((int) btoc(NCARGS))), bno);
	iput(ip);
}

/*
 * Read in and set up memory for executed file.
 */
getxfile(ip, nargc, uid, gid)
	register struct inode *ip;
	int nargc, uid, gid;
{
	register size_t ts, ds, ss;
	int pagi;

	if (u.u_exdata.ux_mag == 0413)
		pagi = SPAGI;
	else
		pagi = 0;
	if (u.u_exdata.ux_tsize!=0 && (ip->i_flag&ITEXT)==0 &&
	    ip->i_count!=1) {
		register struct file *fp;

		for (fp = file; fp < fileNFILE; fp++) {
			if (fp->f_type == DTYPE_INODE &&
			    fp->f_count > 0 &&
			    (struct inode *)fp->f_data == ip &&
			    (fp->f_flag&FWRITE)) {
				u.u_error = ETXTBSY;
				goto bad;
			}
		}
	}

	/*
	 * Compute text and data sizes and make sure not too large.
	 */
	ts = clrnd(btoc(u.u_exdata.ux_tsize));
	ds = clrnd(btoc((u.u_exdata.ux_dsize+u.u_exdata.ux_bsize)));
	ss = clrnd(SSIZE + btoc(nargc));
	if (chksize((unsigned)ts, (unsigned)ds, (unsigned)ss))
		goto bad;

	/*
	 * Make sure enough space to start process.
	 */
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
	u.u_procp->p_flag &= ~(SPAGI|SSEQL|SUANOM|SOUSIG);
	u.u_procp->p_flag |= pagi;
	u.u_dmap = u.u_cdmap;
	u.u_smap = u.u_csmap;
	vgetvm(ts, ds, ss);

	if (pagi == 0)
		u.u_error =
		    rdwri(UIO_READ, ip,
			(char *)ctob(dptov(u.u_procp, 0)),
			(int)u.u_exdata.ux_dsize,
			(int)(sizeof(u.u_exdata)+u.u_exdata.ux_tsize),
			0, (int *)0);
	xalloc(ip, pagi);
	if (pagi && u.u_procp->p_textp)
		vinifod((struct fpte *)dptopte(u.u_procp, 0),
		    PG_FTEXT, u.u_procp->p_textp->x_iptr,
		    (long)(1 + ts/CLSIZE), (int)btoc(u.u_exdata.ux_dsize));

#ifdef vax
	/* THIS SHOULD BE DONE AT A LOWER LEVEL, IF AT ALL */
	mtpr(TBIA, 0);
#endif

	if (u.u_error)
		swkill(u.u_procp, "i/o error mapping pages");
	/*
	 * set SUID/SGID protections, if no tracing
	 */
	if ((u.u_procp->p_flag&STRC)==0) {
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_gid = gid;
	} else
		psignal(u.u_procp, SIGTRAP);
	u.u_tsize = ts;
	u.u_dsize = ds;
	u.u_ssize = ss;
	u.u_prof.pr_scale = 0;
bad:
	return;
}

/*
 * Clear registers on exec
 */
setregs()
{
	register int i;
	register struct proc *p = u.u_procp;

	/*
	 * Reset caught signals.  Held signals
	 * remain held through p_sigmask.
	 */
	while (p->p_sigcatch) {
		(void) spl6();
		i = ffs(p->p_sigcatch);
		p->p_sigcatch &= ~(1 << (i - 1));
		u.u_signal[i] = SIG_DFL;
		(void) spl0();
	}
#ifdef notdef
	/* should pass args to init on the stack */
	for (rp = &u.u_ar0[0]; rp < &u.u_ar0[16];)
		*rp++ = 0;
#endif
	u.u_ar0[PC] = u.u_exdata.ux_entloc+2;
	for (i=0; i<NOFILE; i++) {
		if (u.u_pofile[i]&UF_EXCLOSE) {
			closef(u.u_ofile[i]);
			u.u_ofile[i] = NULL;
			u.u_pofile[i] = 0;
		}
		u.u_pofile[i] &= ~UF_MAPPED;
	}

	/*
	 * Remember file name for accounting.
	 */
	u.u_acflag &= ~AFORK;
	bcopy((caddr_t)u.u_dent.d_name, (caddr_t)u.u_comm,
	    (unsigned)(u.u_dent.d_namlen + 1));
}
