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
 *	@(#)kern_ktrace.c	1.1 (Berkeley) %G%
 */

#ifdef KTRACE

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "assym.s"
#include "proc.h"
#include "seg.h"
#include "acct.h"
#include "fs.h"
#include "inode.h"
#include "syslog.h"
#include "kernel.h"
#include "ktrace.h"
#include "malloc.h"

#include "../sys/syscalls.c"

extern int nsysent;
extern char *syscallnames[];

struct ktr_header *
ktrgetheader(type)
{
	register struct ktr_header *kth;

	MALLOC(kth, struct ktr_header *, sizeof (struct ktr_header), 
		M_TEMP, M_WAITOK);
	if (kth == NULL)
		return (NULL);
	kth->ktr_type = type;
	kth->ktr_time = time;
	kth->ktr_pid = u.u_procp->p_pid;
	bcopy(u.u_comm, kth->ktr_comm, MAXCOMLEN);

	return (kth);
}

ktrsyscall(ip, code, narg)
	struct inode *ip;
{
	struct	ktr_header *kth = ktrgetheader(KTR_SYSCALL);
	struct	ktr_syscall *ktp;
	register len = sizeof(struct ktr_syscall) + (narg * sizeof(int));
	int 	*argp, i;

	if (kth == NULL) {
		printf("lost syscall trace - no header\n");	/* DEBUG */
		return;
	}
	MALLOC(ktp, struct ktr_syscall *, len, M_TEMP, M_WAITOK);
	if (ktp == NULL) {
		printf("lost syscall trace - no buffer\n");	/* DEBUG */
		FREE(kth, M_TEMP);
		return;
	}
	ktp->ktr_code = code;
	ktp->ktr_narg = narg;
	argp = (int *)((char *)ktp + sizeof(struct ktr_syscall));
	for (i = 0; i < narg; i++)
		*argp++ = u.u_arg[i];
	kth->ktr_buf = (caddr_t)ktp;
	kth->ktr_len = len;
	ktrwrite(ip, kth);
	FREE(ktp, M_TEMP);
	FREE(kth, M_TEMP);
}

ktrsysret(ip, code)
	struct inode *ip;
{
	struct ktr_header *kth = ktrgetheader(KTR_SYSRET);
	struct ktr_sysret *ktp;

	if (kth == NULL) {
		printf("lost syscall ret - no header\n");	/* DEBUG */
		return;
	}
	MALLOC(ktp, struct ktr_sysret *, sizeof(struct ktr_sysret),
		M_TEMP , M_WAITOK);
	if (ktp == NULL) {
		printf("lost syscall ret - no buffer\n");	/* DEBUG */
		FREE(kth, M_TEMP);
		return;
	}
	ktp->ktr_code = code;
	ktp->ktr_eosys = u.u_eosys;
	ktp->ktr_error = u.u_error;
	ktp->ktr_retval = u.u_r.r_val1;		/* what about val2 ? */

	kth->ktr_buf = (caddr_t)ktp;
	kth->ktr_len = sizeof(struct ktr_sysret);

	ktrwrite(ip, kth);
	FREE(ktp, M_TEMP);
	FREE(kth, M_TEMP);
}

ktrnamei(ip, path)
	struct inode *ip;
	char *path;
{
	struct ktr_header *kth = ktrgetheader(KTR_NAMEI);

	if (kth == NULL) {
		printf("lost namei - no header\n");	/* DEBUG */
		return;
	}
	kth->ktr_len = strlen(path);
	kth->ktr_buf = path;

	ktrwrite(ip, kth);
	FREE(kth, M_TEMP);
}

/*
 * ktrace system call
 */
ktrace()
{
	register struct inode *ip = NULL;
	register struct a {
		char	*fname;
		int	ops;
		int	facs;
		pid_t	pid;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct proc *p;
	struct pgrp *pg;
	register int ops = uap->ops&0x3;
	register int facs = uap->facs;

	/*
	 * Until security implications are thought through,
	 * limit tracing to root.
	 */
	if (!suser()) {
		u.u_error = EACCES;
		return;
	}
	if (ops != KTROP_CLEAR) {
		/*
		 * an operation which requires a file argument.
		 */
		ndp->ni_nameiop = LOOKUP | FOLLOW;
		ndp->ni_segflg = UIO_USERSPACE;
		ndp->ni_dirp = uap->fname;
		ip = namei(ndp);
		if (ip == NULL)
			return;
		if ((ip->i_mode&IFMT) != IFREG) {
			u.u_error = EACCES;
			iput(ip);
			return;
		}
		if (ip->i_fs->fs_ronly) {
			u.u_error = EROFS;
			iput(ip);
			return;
		}
		iunlock(ip);
	}
	/*
	 * Clear all uses of the tracefile
	 */
	if (ops == KTROP_CLEARFILE) {
		for (p = allproc; p != NULL; p = p->p_nxt) {
			if (p->p_tracep == ip) {
				p->p_flag &= ~SKTR;
				p->p_tracep = NULL;
				p->p_traceflag = 0;
				irele(ip);
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
				ktrsetchildren(p, ops, facs, ip);
			else 
				ktrops(p, ops, facs, ip);
					
	} else {
		p = pfind(uap->pid);
		if (p == NULL) {
			u.u_error = ESRCH;
			goto done;
		}
		if (uap->ops&KTROP_INHERITFLAG)
			ktrsetchildren(p, ops, facs, ip);
		else
			ktrops(p, ops, facs, ip);
	}
done:
	if (ip != NULL)
		irele(ip);
}

ktrops(p, ops, facs, ip)
	struct proc *p;
	struct inode *ip;
{
	if (ops == KTROP_SET) {
		if (p->p_tracep != ip) {
			/*
			 * if trace file already in use, relinquish
			 */
			if (p->p_tracep != NULL)
				irele(p->p_tracep);
			igrab(ip);
			p->p_tracep = ip;
			iunlock(ip);
		}
		p->p_traceflag |= facs;
	} else {	
		/* KTROP_CLEAR */
		if ((p->p_traceflag &= ~facs) == 0) {
			if (p->p_tracep != NULL) {
				irele(p->p_tracep);
				p->p_tracep = NULL;
			}
			p->p_flag &= SKTR;
		}
	}
}
		
ktrsetchildren(top, ops, facs, ip)
	struct proc *top;
	struct inode *ip;
{
	register struct proc *p;
	register int ndx;

	p = top;
	for (;;) {
		if (ops == KTROP_SET)
			p->p_flag |= SKTR;
		ktrops(p, ops, facs, ip);
		/*
		 * If this process has children, descend to them next,
		 * otherwise do any siblings, and if done with this level,
		 * follow back up the tree (but not past top).
		 */
		if (p->p_cptr)
			p = p->p_cptr;
		else if (p == top)
			return;
		else if (p->p_osptr)
			p = p->p_osptr;
		else for (;;) {
			p = p->p_pptr;
			if (p == top)
				return;
			if (p->p_osptr) {
				p = p->p_osptr;
				break;
			}
		}
	}
}

ktrwrite(ip, kth)
	register struct inode *ip;
	struct ktr_header *kth;
{
	int save = u.u_error;
	int osize;
	
	ilock(ip);
	osize = ip->i_size;
	u.u_error = 0;
	u.u_error = rdwri(UIO_WRITE, ip, (caddr_t)kth, 
			sizeof(struct ktr_header), ip->i_size, 1, (int *)0);
	if (u.u_error) {
		itrunc(ip, (u_long)osize);
		goto end;
	}
	if (kth->ktr_len > 0) {
		u.u_error = rdwri(UIO_WRITE, ip, kth->ktr_buf,
			    kth->ktr_len, ip->i_size, 1, (int *)0);
		if (u.u_error)
			itrunc(ip, (u_long)osize);
	}
end:
	u.u_error = save;
	iunlock(ip);
}

#endif
