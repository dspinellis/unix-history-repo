/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_exec.c	7.30 (Berkeley) 6/30/90
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "mount.h"
#include "ucred.h"
#include "malloc.h"
#include "buf.h"
#include "vnode.h"
#include "seg.h"
#include "vm.h"
#include "text.h"
#include "file.h"
#include "uio.h"
#include "acct.h"
#include "exec.h"

#include "machine/reg.h"
#include "machine/pte.h"
#include "machine/psl.h"
#include "machine/mtpr.h"

#ifdef HPUXCOMPAT
#include "../hpux/hpux_exec.h"
#endif

/*
 * exec system call, with and without environments.
 */
execv(p, uap, retval)
	struct proc *p;
	struct args {
		char	*fname;
		char	**argp;
		char	**envp;
	} *uap;
	int *retval;
{

	uap->envp = NULL;
	return (execve(p, uap, retval));
}

/* ARGSUSED */
execve(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		char	**argp;
		char	**envp;
	} *uap;
	int *retval;
{
	register nc;
	register char *cp;
	register struct buf *bp;
	struct buf *tbp;
	int na, ne, ucp, ap, cc;
	unsigned len;
	int indir, uid, gid;
	char *sharg;
	struct vnode *vp;
	swblk_t bno;
	struct vattr vattr;
	char cfname[MAXCOMLEN + 1];
	char cfarg[MAXINTERP];
	union {
		char	ex_shell[MAXINTERP];	/* #! and interpreter name */
		struct	exec ex_exec;
#ifdef HPUXCOMPAT
		struct	hpux_exec ex_hexec;
#endif
	} exdata;
#ifdef HPUXCOMPAT
	struct hpux_exec hhead;
#endif
	register struct ucred *cred = u.u_cred;
	register struct nameidata *ndp = &u.u_nd;
	int resid, error, flags = 0;

  start:
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		return (error);
	vp = ndp->ni_vp;
	bno = 0;
	bp = 0;
	indir = 0;
	uid = cred->cr_uid;
	gid = cred->cr_gid;
	if (error = VOP_GETATTR(vp, &vattr, cred))
		goto bad;
	if (vp->v_mount->mnt_flag & MNT_NOEXEC) {
		error = EACCES;
		goto bad;
	}
	if ((vp->v_mount->mnt_flag & MNT_NOSUID) == 0) {
		if (vattr.va_mode & VSUID)
			uid = vattr.va_uid;
		if (vattr.va_mode & VSGID)
			gid = vattr.va_gid;
	}

  again:
	if (error = VOP_ACCESS(vp, VEXEC, cred))
		goto bad;
	if ((p->p_flag & STRC) && (error = VOP_ACCESS(vp, VREAD, cred)))
		goto bad;
	if (vp->v_type != VREG ||
	    (vattr.va_mode & (VEXEC|(VEXEC>>3)|(VEXEC>>6))) == 0) {
		error = EACCES;
		goto bad;
	}

	/*
	 * Read in first few bytes of file for segment sizes, magic number:
	 *	OMAGIC = plain executable
	 *	NMAGIC = RO text
	 *	ZMAGIC = demand paged RO text
	 * Also an ASCII line beginning with #! is
	 * the file name of a ``shell'' and arguments may be prepended
	 * to the argument list if given here.
	 *
	 * SHELL NAMES ARE LIMITED IN LENGTH.
	 *
	 * ONLY ONE ARGUMENT MAY BE PASSED TO THE SHELL FROM
	 * THE ASCII LINE.
	 */
	exdata.ex_shell[0] = '\0';	/* for zero length files */
	error = vn_rdwr(UIO_READ, vp, (caddr_t)&exdata, sizeof (exdata),
	    (off_t)0, UIO_SYSSPACE, (IO_UNIT|IO_NODELOCKED), cred, &resid);
	if (error)
		goto bad;
#ifndef lint
	if (resid > sizeof(exdata) - sizeof(exdata.ex_exec) &&
	    exdata.ex_shell[0] != '#') {
		error = ENOEXEC;
		goto bad;
	}
#endif
#if defined(hp300)
	switch ((int)exdata.ex_exec.a_mid) {

	/*
	 * An ancient hp200 or hp300 binary, shouldn't happen anymore.
	 * Mark as invalid.
	 */
	case MID_ZERO:
		exdata.ex_exec.a_magic = 0;
		break;

	/*
	 * HP200 series has a smaller page size so we cannot
	 * demand-load or even write protect text, so we just
	 * treat as OMAGIC.
	 */
	case MID_HP200:
		exdata.ex_exec.a_magic = OMAGIC;
		break;

	case MID_HP300:
		break;

#ifdef HPUXCOMPAT
	case MID_HPUX:
		/*
		 * Save a.out header.  This is eventually saved in the pcb,
		 * but we cannot do that yet in case the exec fails before
		 * the image is overlayed.
		 */
		bcopy((caddr_t)&exdata.ex_hexec,
		      (caddr_t)&hhead, sizeof hhead);
		/*
		 * If version number is 0x2bad this is a native BSD
		 * binary created via the HPUX SGS.  Should not be
		 * treated as an HPUX binary.
		 */
		if (exdata.ex_hexec.ha_version != BSDVNUM)
			flags |= SHPUX;
		/*
		 * Shuffle important fields to their BSD locations.
		 * Note that the order in which this is done is important.
		 */
		exdata.ex_exec.a_text = exdata.ex_hexec.ha_text;
		exdata.ex_exec.a_data = exdata.ex_hexec.ha_data;
		exdata.ex_exec.a_bss = exdata.ex_hexec.ha_bss;
		exdata.ex_exec.a_entry = exdata.ex_hexec.ha_entry;
		/*
		 * For ZMAGIC files, make sizes consistant with those
		 * generated by BSD ld.
		 */
		if (exdata.ex_exec.a_magic == ZMAGIC) {
			exdata.ex_exec.a_text = 
				ctob(btoc(exdata.ex_exec.a_text));
			nc = exdata.ex_exec.a_data + exdata.ex_exec.a_bss;
			exdata.ex_exec.a_data =
				ctob(btoc(exdata.ex_exec.a_data));
			nc -= (int)exdata.ex_exec.a_data;
			exdata.ex_exec.a_bss = (nc < 0) ? 0 : nc;
		}
		break;
#endif
	}
#endif
	switch ((int)exdata.ex_exec.a_magic) {

	case OMAGIC:
		exdata.ex_exec.a_data += exdata.ex_exec.a_text;
		exdata.ex_exec.a_text = 0;
		break;

	case ZMAGIC:
		flags |= SPAGV;
	case NMAGIC:
		if (exdata.ex_exec.a_text == 0) {
			error = ENOEXEC;
			goto bad;
		}
		break;

	default:
		if (exdata.ex_shell[0] != '#' ||
		    exdata.ex_shell[1] != '!' ||
		    indir) {
			error = ENOEXEC;
			goto bad;
		}
		for (cp = &exdata.ex_shell[2];; ++cp) {
			if (cp >= &exdata.ex_shell[MAXINTERP]) {
				error = ENOEXEC;
				goto bad;
			}
			if (*cp == '\n') {
				*cp = '\0';
				break;
			}
			if (*cp == '\t')
				*cp = ' ';
		}
		cp = &exdata.ex_shell[2];
		while (*cp == ' ')
			cp++;
		ndp->ni_dirp = cp;
		while (*cp && *cp != ' ')
			cp++;
		cfarg[0] = '\0';
		if (*cp) {
			*cp++ = '\0';
			while (*cp == ' ')
				cp++;
			if (*cp)
				bcopy((caddr_t)cp, (caddr_t)cfarg, MAXINTERP);
		}
		indir = 1;
		vput(vp);
		ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
		ndp->ni_segflg = UIO_SYSSPACE;
		if (error = namei(ndp))
			return (error);
		vp = ndp->ni_vp;
		if (error = VOP_GETATTR(vp, &vattr, cred))
			goto bad;
		bcopy((caddr_t)ndp->ni_dent.d_name, (caddr_t)cfname,
		    MAXCOMLEN);
		cfname[MAXCOMLEN] = '\0';
		uid = cred->cr_uid;	/* shell scripts can't be setuid */
		gid = cred->cr_gid;
		goto again;
	}
	/*
	 * If the vnode has been modified since we last used it,
	 * then throw away all its pages and its text table entry.
	 */
	if (vp->v_text && vp->v_text->x_mtime != vattr.va_mtime.tv_sec) {
		/*
		 * Try once to release, if it is still busy
		 * take more drastic action.
		 */
		xrele(vp);
		if (vp->v_flag & VTEXT) {
			vput(vp);
			vgone(vp);
			goto start;
		}
	}

	/*
	 * Collect arguments on "file" in swap space.
	 */
	na = 0;
	ne = 0;
	nc = 0;
	cc = 0;
	bno = rmalloc(argmap, (long)ctod(clrnd((int)btoc(NCARGS))));
	if (bno == 0) {
		swkill(p, "exec: no swap space");
		goto bad;
	}
	if (bno % CLSIZE)
		panic("execa rmalloc");
#ifdef GENERIC
	if (rootdev == dumpdev)
		bno += 4096;
#endif
	/*
	 * Copy arguments into file in argdev area.
	 */
	if (uap->argp) for (;;) {
		ap = NULL;
		sharg = NULL;
		if (indir && na == 0) {
			sharg = cfname;
			ap = (int)sharg;
			uap->argp++;		/* ignore argv[0] */
		} else if (indir && (na == 1 && cfarg[0])) {
			sharg = cfarg;
			ap = (int)sharg;
		} else if (indir && (na == 1 || na == 2 && cfarg[0]))
			ap = (int)uap->fname;
		else if (uap->argp) {
			ap = fuword((caddr_t)uap->argp);
			uap->argp++;
		}
		if (ap == NULL && uap->envp) {
			uap->argp = NULL;
			if ((ap = fuword((caddr_t)uap->envp)) != NULL)
				uap->envp++, ne++;
		}
		if (ap == NULL)
			break;
		na++;
		if (ap == -1) {
			error = EFAULT;
			if (bp) {
				brelse(bp);
				bp = 0;
			}
			goto badarg;
		}
		do {
			if (cc <= 0) {
				/*
				 * We depend on NCARGS being a multiple of
				 * CLBYTES.  This way we need only check
				 * overflow before each buffer allocation.
				 */
				if (nc >= NCARGS-1) {
					error = E2BIG;
					break;
				}
				if (bp)
					bdwrite(bp);
				cc = CLBYTES;
				bp = getblk(argdev_vp, bno + ctod(nc/NBPG), cc);
				cp = bp->b_un.b_addr;
			}
			if (sharg) {
				error = copystr(sharg, cp, (unsigned)cc, &len);
				sharg += len;
			} else {
				error = copyinstr((caddr_t)ap, cp, (unsigned)cc,
				    &len);
				ap += len;
			}
			cp += len;
			nc += len;
			cc -= len;
		} while (error == ENOENT);
		if (error) {
			if (bp)
				brelse(bp);
			bp = 0;
			goto badarg;
		}
	}
	if (bp)
		bdwrite(bp);
	bp = 0;
	nc = (nc + NBPW-1) & ~(NBPW-1);
	error = getxfile(p, vp, &exdata.ex_exec, flags, nc + (na+4)*NBPW,
	    uid, gid);
	if (error) {
badarg:
		for (cc = 0; cc < nc; cc += CLBYTES) {
			(void) baddr(argdev_vp, bno + ctod(cc/NBPG),
				CLBYTES, NOCRED, &tbp);
			bp = tbp;
			if (bp) {
				bp->b_flags |= B_INVAL;		/* throw away */
				brelse(bp);
				bp = 0;
			}
		}
		goto bad;
	}
	if (vp->v_text)
		vp->v_text->x_mtime = vattr.va_mtime.tv_sec;
	vput(vp);
	vp = NULL;

#ifdef HPUXCOMPAT
	/*
	 * We are now committed to the exec so we can save the exec
	 * header in the pcb where we can dump it if necessary in core()
	 */
	if (u.u_pcb.pcb_flags & PCB_HPUXBIN)
		bcopy((caddr_t)&hhead,
		      (caddr_t)u.u_pcb.pcb_exec, sizeof hhead);
#endif

	/*
	 * Copy back arglist.
	 */
	ucp = USRSTACK - nc - NBPW;
	ap = ucp - na*NBPW - 3*NBPW;
	u.u_ar0[SP] = ap;
	(void) suword((caddr_t)ap, na-ne);
	nc = 0;
	cc = 0;
	for (;;) {
		ap += NBPW;
		if (na == ne) {
			(void) suword((caddr_t)ap, 0);
			ap += NBPW;
		}
		if (--na < 0)
			break;
		(void) suword((caddr_t)ap, ucp);
		do {
			if (cc <= 0) {
				if (bp)
					brelse(bp);
				cc = CLBYTES;
				error = bread(argdev_vp,
				    (daddr_t)(bno + ctod(nc / NBPG)), cc,
				    NOCRED, &tbp);
				bp = tbp;
				bp->b_flags |= B_INVAL;		/* throw away */
				cp = bp->b_un.b_addr;
			}
			error = copyoutstr(cp, (caddr_t)ucp, (unsigned)cc,
			    &len);
			ucp += len;
			cp += len;
			nc += len;
			cc -= len;
		} while (error == ENOENT);
		if (error == EFAULT)
			panic("exec: EFAULT");
	}
	(void) suword((caddr_t)ap, 0);

	execsigs(p);

	for (nc = u.u_lastfile; nc >= 0; --nc) {
		if (u.u_pofile[nc] & UF_EXCLOSE) {
			(void) closef(u.u_ofile[nc]);
			u.u_ofile[nc] = NULL;
			u.u_pofile[nc] = 0;
		}
		u.u_pofile[nc] &= ~UF_MAPPED;
	}
	while (u.u_lastfile >= 0 && u.u_ofile[u.u_lastfile] == NULL)
		u.u_lastfile--;
	setregs(exdata.ex_exec.a_entry, retval);
	/*
	 * Remember file name for accounting.
	 */
	u.u_acflag &= ~AFORK;
	if (indir)
		bcopy((caddr_t)cfname, (caddr_t)p->p_comm, MAXCOMLEN);
	else {
		if (ndp->ni_dent.d_namlen > MAXCOMLEN)
			ndp->ni_dent.d_namlen = MAXCOMLEN;
		bcopy((caddr_t)ndp->ni_dent.d_name, (caddr_t)p->p_comm,
		    (unsigned)(ndp->ni_dent.d_namlen + 1));
	}
bad:
	if (bp)
		brelse(bp);
	if (bno)
		rmfree(argmap, (long)ctod(clrnd((int) btoc(NCARGS))), bno);
	if (vp)
		vput(vp);
	return (error);
}

/*
 * Read in and set up memory for executed file.
 */
getxfile(p, vp, ep, flags, nargc, uid, gid)
	register struct proc *p;
	register struct vnode *vp;
	register struct exec *ep;
	int flags, nargc, uid, gid;
{
	segsz_t ts, ds, ids, uds, ss;
	register struct ucred *cred = u.u_cred;
	off_t toff;
	int error;

#ifdef HPUXCOMPAT
	if (ep->a_mid == MID_HPUX)
		toff = sizeof (struct hpux_exec);
	else
#endif
	toff = sizeof (struct exec);
	if (vp->v_text && (vp->v_text->x_flag & XTRC))
		return (ETXTBSY);
	if (ep->a_text != 0 && (vp->v_flag & VTEXT) == 0 &&
	    vp->v_usecount != 1) {
		register struct file *fp;

		for (fp = file; fp < fileNFILE; fp++) {
			if (fp->f_type == DTYPE_VNODE &&
			    fp->f_count > 0 &&
			    (struct vnode *)fp->f_data == vp &&
			    (fp->f_flag & FWRITE)) {
				return (ETXTBSY);
			}
		}
	}

	/*
	 * Compute text and data sizes and make sure not too large.
	 * NB - Check data and bss separately as they may overflow 
	 * when summed together.
	 */
	ts = clrnd(btoc(ep->a_text));
	ids = clrnd(btoc(ep->a_data));
	uds = clrnd(btoc(ep->a_bss));
	ds = clrnd(btoc(ep->a_data + ep->a_bss));
	ss = clrnd(SSIZE + btoc(nargc));
	if (error =
	    chksize((unsigned)ts, (unsigned)ids, (unsigned)uds, (unsigned)ss))
		return (error);

	/*
	 * Make sure enough space to start process.
	 */
	u.u_cdmap = zdmap;
	u.u_csmap = zdmap;
	if (error = swpexpand(ds, ss, &u.u_cdmap, &u.u_csmap))
		return (error);

	/*
	 * At this point, we are committed to the new image!
	 * Release virtual memory resources of old process, and
	 * initialize the virtual memory of the new process.
	 * If we resulted from vfork(), instead wakeup our
	 * parent who will set SVFDONE when he has taken back
	 * our resources.
	 */
	if ((p->p_flag & SVFORK) == 0) {
#ifdef MAPMEM
		if (u.u_mmap && (error = mmexec(p)))
			return (error);
#endif
		vrelvm();
	} else {
		p->p_flag &= ~SVFORK;
		p->p_flag |= SKEEP;
		wakeup((caddr_t)p);
		while ((p->p_flag & SVFDONE) == 0)
			sleep((caddr_t)p, PZERO - 1);
		p->p_flag &= ~(SVFDONE|SKEEP);
	}
#ifdef hp300
	u.u_pcb.pcb_flags &= ~(PCB_AST|PCB_HPUXMMAP|PCB_HPUXBIN);
#ifdef HPUXCOMPAT
	/* remember that we were loaded from an HPUX format file */
	if (ep->a_mid == MID_HPUX)
		u.u_pcb.pcb_flags |= PCB_HPUXBIN;
#endif
#endif
	p->p_flag &= ~(SPAGV|SSEQL|SUANOM|SHPUX);
	p->p_flag |= flags | SEXEC;
	u.u_dmap = u.u_cdmap;
	u.u_smap = u.u_csmap;
	vgetvm(ts, ds, ss);

	if ((flags & SPAGV) == 0)
		(void) vn_rdwr(UIO_READ, vp,
			(char *)ctob(dptov(p, 0)),
			(int)ep->a_data,
			(off_t)(toff + ep->a_text),
			UIO_USERSPACE, (IO_UNIT|IO_NODELOCKED), cred, (int *)0);
	xalloc(vp, ep, toff, cred);
#if defined(tahoe)
	/*
	 * Define new keys.
	 */
	if (p->p_textp == 0) {	/* use existing code key if shared */
		ckeyrelease(p->p_ckey);
		p->p_ckey = getcodekey();
	}
	mtpr(CCK, p->p_ckey);
	dkeyrelease(p->p_dkey);
	p->p_dkey = getdatakey();
	mtpr(DCK, p->p_dkey);
#endif
	if ((flags & SPAGV) && p->p_textp)
		vinifod(p, (struct fpte *)dptopte(p, 0),
		    PG_FTEXT, p->p_textp->x_vptr,
		    (long)(1 + ts/CLSIZE), (segsz_t)btoc(ep->a_data));

#if defined(vax) || defined(tahoe)
	/* THIS SHOULD BE DONE AT A LOWER LEVEL, IF AT ALL */
	mtpr(TBIA, 0);
#endif
#ifdef hp300
	TBIAU();
#endif
#if defined(i386)
	tlbflush();
#endif

	/*
	 * set SUID/SGID protections, if no tracing
	 */
	if ((p->p_flag&STRC)==0) {
		if (uid != cred->cr_uid || gid != cred->cr_gid)
			u.u_cred = cred = crcopy(cred);
		cred->cr_uid = uid;
		cred->cr_gid = gid;
		p->p_uid = uid;
	} else
		psignal(p, SIGTRAP);
	p->p_svuid = p->p_uid;
	p->p_svgid = cred->cr_gid;
	u.u_tsize = ts;
	u.u_dsize = ds;
	u.u_ssize = ss;
	u.u_prof.pr_scale = 0;
#if defined(tahoe)
	u.u_pcb.pcb_savacc.faddr = (float *)NULL;
#endif
	return (0);
}
