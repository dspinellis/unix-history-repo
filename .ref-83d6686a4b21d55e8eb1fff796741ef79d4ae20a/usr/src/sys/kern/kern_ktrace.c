/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_ktrace.c	1.4 (Berkeley) %G%
 */

#ifdef KTRACE

#include "param.h"
#include "user.h"
#include "proc.h"
#include "file.h"
#include "vnode.h"
#include "ktrace.h"
#include "malloc.h"

#include "syscalls.c"

extern int nsysent;
extern char *syscallnames[];

int ktrace_nocheck = 1;

struct ktr_header *
ktrgetheader(type)
{
	register struct ktr_header *kth;

	MALLOC(kth, struct ktr_header *, sizeof (struct ktr_header), 
		M_TEMP, M_WAITOK);
	kth->ktr_type = type;
	microtime(&kth->ktr_time);
	kth->ktr_pid = u.u_procp->p_pid;
	bcopy(u.u_comm, kth->ktr_comm, MAXCOMLEN);
	return (kth);
}

ktrsyscall(vp, code, narg)
	struct vnode *vp;
{
	struct	ktr_header *kth = ktrgetheader(KTR_SYSCALL);
	struct	ktr_syscall *ktp;
	register len = sizeof(struct ktr_syscall) + (narg * sizeof(int));
	int 	*argp, i;

	if (kth == NULL)
		return;
	MALLOC(ktp, struct ktr_syscall *, len, M_TEMP, M_WAITOK);
	ktp->ktr_code = code;
	ktp->ktr_narg = narg;
	argp = (int *)((char *)ktp + sizeof(struct ktr_syscall));
	for (i = 0; i < narg; i++)
		*argp++ = u.u_arg[i];
	kth->ktr_buf = (caddr_t)ktp;
	kth->ktr_len = len;
	ktrwrite(vp, kth);
	FREE(ktp, M_TEMP);
	FREE(kth, M_TEMP);
}

ktrsysret(vp, code)
	struct vnode *vp;
{
	struct ktr_header *kth = ktrgetheader(KTR_SYSRET);
	struct ktr_sysret *ktp;

	if (kth == NULL)
		return;
	MALLOC(ktp, struct ktr_sysret *, sizeof(struct ktr_sysret),
		M_TEMP , M_WAITOK);
	ktp->ktr_code = code;
	ktp->ktr_eosys = u.u_eosys;
	ktp->ktr_error = u.u_error;
	ktp->ktr_retval = u.u_r.r_val1;		/* what about val2 ? */

	kth->ktr_buf = (caddr_t)ktp;
	kth->ktr_len = sizeof(struct ktr_sysret);

	ktrwrite(vp, kth);
	FREE(ktp, M_TEMP);
	FREE(kth, M_TEMP);
}

ktrnamei(vp, path)
	struct vnode *vp;
	char *path;
{
	struct ktr_header *kth = ktrgetheader(KTR_NAMEI);

	if (kth == NULL)
		return;
	kth->ktr_len = strlen(path);
	kth->ktr_buf = path;

	ktrwrite(vp, kth);
	FREE(kth, M_TEMP);
}

ktrgenio(vp, fd, rw, iov, len)
	struct vnode *vp;
	enum uio_rw rw;
	register struct iovec *iov;
{
	struct ktr_header *kth = ktrgetheader(KTR_GENIO);
	register struct ktr_genio *ktp;
	register caddr_t cp;
	register int resid = len, cnt;
	
	if (kth == NULL || u.u_error)
		return;
	MALLOC(ktp, struct ktr_genio *, sizeof(struct ktr_genio) + len,
		M_TEMP, M_WAITOK);
	ktp->ktr_fd = fd;
	ktp->ktr_rw = rw;
	cp = (caddr_t)((char *)ktp + sizeof (struct ktr_genio));
	while (resid > 0) {
		if ((cnt = iov->iov_len) > resid)
			cnt = resid;
		if (copyin(iov->iov_base, cp, cnt))
			goto done;
		cp += cnt;
		resid -= cnt;
		iov++;
	}
	kth->ktr_buf = (caddr_t)ktp;
	kth->ktr_len = sizeof (struct ktr_genio) + len;

	ktrwrite(vp, kth);
done:
	FREE(kth, M_TEMP);
	FREE(ktp, M_TEMP);
}

/*
 * ktrace system call
 */
ktrace()
{
	register struct a {
		char	*fname;
		int	ops;
		int	facs;
		int	pid;
	} *uap = (struct a *)u.u_ap;
	register struct vnode *vp = NULL;
	register struct nameidata *ndp = &u.u_nd;
	register struct proc *p;
	struct pgrp *pg;
	register int ops = uap->ops&0x3;
	register int facs = uap->facs;
	register int ret = 0;

	/*
	 * Until security implications are thought through,
	 * limit tracing to root (unless ktrace_nocheck is set).
	 */
	if (!ktrace_nocheck && (u.u_error = suser(u.u_cred, &u.u_acflag)))
		return;
	if (ops != KTROP_CLEAR) {
		/*
		 * an operation which requires a file argument.
		 */
		ndp->ni_segflg = UIO_USERSPACE;
		ndp->ni_dirp = uap->fname;
		if (u.u_error = vn_open(ndp, FREAD|FWRITE, 0))
			return;
		vp = ndp->ni_vp;
		if (vp->v_type != VREG) {
			u.u_error = EACCES;
			vrele(vp);
			return;
		}
	}
	/*
	 * Clear all uses of the tracefile
	 */
	if (ops == KTROP_CLEARFILE) {
		for (p = allproc; p != NULL; p = p->p_nxt) {
			if (p->p_tracep == vp) {
				p->p_flag &= ~SKTR;
				p->p_tracep = NULL;
				p->p_traceflag = 0;
				vrele(vp);
			}
		}
		goto done;
	}

	/*
	 * need something to (un)trace
	 */
	if (!facs) {
		u.u_error = EINVAL;
		goto done;
	}

	if (uap->pid < 0) {
		pg = pgfind(-uap->pid);
		if (pg == NULL) {
			u.u_error = ESRCH;
			goto done;
		}
		for (p = pg->pg_mem; p != NULL; p = p->p_pgrpnxt)
			if (uap->ops&KTROP_INHERITFLAG)
				ret |= ktrsetchildren(p, ops, facs, vp);
			else 
				ret |= ktrops(p, ops, facs, vp);
					
	} else {
		p = pfind(uap->pid);
		if (p == NULL) {
			u.u_error = ESRCH;
			goto done;
		}
		if (uap->ops&KTROP_INHERITFLAG)
			ret |= ktrsetchildren(p, ops, facs, vp);
		else
			ret |= ktrops(p, ops, facs, vp);
	}
	if (!ret)
		u.u_error = EPERM;
done:
	if (vp != NULL)
		vrele(vp);
}

ktrops(p, ops, facs, vp)
	struct proc *p;
	struct vnode *vp;
{

	if (u.u_uid && u.u_uid != p->p_uid)
		return 0;
	if (ops == KTROP_SET) {
		if (p->p_tracep != vp) {
			/*
			 * if trace file already in use, relinquish
			 */
			if (p->p_tracep != NULL)
				vrele(p->p_tracep);
			if (vp->v_count == 0)
				panic("ktrace: bad vnode");
			p->p_tracep = vp;
		}
		p->p_traceflag |= facs;
	} else {	
		/* KTROP_CLEAR */
		if ((p->p_traceflag &= ~facs) == 0) {
			if (p->p_tracep != NULL) {
				vrele(p->p_tracep);
				p->p_tracep = NULL;
			}
			p->p_flag &= ~SKTR;
		}
	}

	return 1;
}

ktrsetchildren(top, ops, facs, vp)
	struct proc *top;
	struct vnode *vp;
{
	register struct proc *p;
	register int ndx;
	register int ret = 0;

	p = top;
	for (;;) {
		if ((ret |= ktrops(p, ops, facs, vp)) && ops == KTROP_SET)
			p->p_flag |= SKTR;
		/*
		 * If this process has children, descend to them next,
		 * otherwise do any siblings, and if done with this level,
		 * follow back up the tree (but not past top).
		 */
		if (p->p_cptr)
			p = p->p_cptr;
		else if (p == top)
			return ret;
		else if (p->p_osptr)
			p = p->p_osptr;
		else for (;;) {
			p = p->p_pptr;
			if (p == top)
				return ret;
			if (p->p_osptr) {
				p = p->p_osptr;
				break;
			}
		}
	}
	/*NOTREACHED*/
}

ktrwrite(vp, kth)
	struct vnode *vp;
	register struct ktr_header *kth;
{
	struct uio auio;
	struct iovec aiov[2];
	int offset, error;

	if (vp == NULL)
		return;
	auio.uio_iov = &aiov[0];
	auio.uio_offset = 0;
	auio.uio_segflg = UIO_SYSSPACE;
	auio.uio_rw = UIO_WRITE;
	aiov[0].iov_base = (caddr_t)kth;
	aiov[0].iov_len = sizeof(struct ktr_header);
	auio.uio_resid = sizeof(struct ktr_header);
	auio.uio_iovcnt = 1;
	if (kth->ktr_len > 0) {
		auio.uio_iovcnt++;
		aiov[1].iov_base = kth->ktr_buf;
		aiov[1].iov_len = kth->ktr_len;
		auio.uio_resid += kth->ktr_len;
	}
	error = VOP_WRITE(vp, &auio, &offset, IO_UNIT|IO_APPEND, u.u_cred);
}
#endif
