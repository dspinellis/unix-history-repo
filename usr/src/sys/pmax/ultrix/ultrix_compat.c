/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_compat.c 1.41 91/04/06$
 *
 *	@(#)ultrix_compat.c	7.3 (Berkeley) %G%
 */

/*
 * Various ULTRIX compatibility routines
 */

#ifdef ULTRIXCOMPAT

#include "param.h"
#include "systm.h"
#include "signalvar.h"
#include "kernel.h"
#include "filedesc.h"
#include "proc.h"
#include "buf.h"
#include "wait.h"
#include "file.h"
#include "namei.h"
#include "vnode.h"
#include "ioctl.h"
#include "ptrace.h"
#include "stat.h"
#include "syslog.h"
#include "malloc.h"
#include "mount.h"
#include "ipc.h"
#include "user.h"

#include "machine/cpu.h"
#include "machine/reg.h"
#include "machine/psl.h"
#include "machine/vmparam.h"

#ifdef DEBUG
int unimpresponse = 0;
#endif

/* YP domainname */
char	domainname[MAXHOSTNAMELEN] = "unknown";
int	domainnamelen = 7;

notimp(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{
	int error = 0;
#ifdef notdef
	register int *argp = uap;
	extern char *ultrixsyscallnames[];

	printf("ULTRIX %s(", ultrixsyscallnames[code]);
	if (nargs)
		while (nargs--)
			printf("%x%c", *argp++, nargs? ',' : ')');
	else
		printf(")");
	printf("\n");
	switch (unimpresponse) {
	case 0:
		error = nosys(p, uap, retval);
		break;
	case 1:
		error = EINVAL;
		break;
	}
#else
	uprintf("ULTRIX system call %d not implemented\n", p->p_md.md_regs[V0]);
	error = nosys(p, uap, retval);
#endif
	return (error);
}

struct wait3_args {
	int	*status;
	int	options;
	int	rusage;
};

ultrixwait3(p, uap, retval)
	struct proc *p;
	struct wait3_args *uap;
	int *retval;
{
	struct {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;
		int	compat;
	} bsd_uap;

	/* rusage pointer must be zero */
	if (uap->rusage)
		return (EINVAL);
	bsd_uap.pid = WAIT_ANY;
	bsd_uap.status = uap->status;
	bsd_uap.options = 0;
	bsd_uap.rusage = 0;
	bsd_uap.compat = 0;
	return (wait1(p, &bsd_uap, retval));
}

struct domainname_args {
	char	*domainname;
	u_int	len;
};

ultrixgetdomainname(p, uap, retval)
	struct proc *p;
	register struct domainname_args *uap;
	int *retval;
{
	if (uap->len > domainnamelen + 1)
		uap->len = domainnamelen + 1;
	return (copyout(domainname, uap->domainname, uap->len));
}

ultrixsetdomainname(p, uap, retval)
	struct proc *p;
	register struct domainname_args *uap;
	int *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if (uap->len > sizeof (domainname) - 1)
		return (EINVAL);
	domainnamelen = uap->len;
	error = copyin(uap->domainname, domainname, uap->len);
	domainname[domainnamelen] = 0;
	return (error);
}

struct getpgrp_args {
	int pid;
};

/*
 * This is the equivalent of BSD getpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid.
 */
ultrixgetpgrp(cp, uap, retval)
	struct proc *cp;
	register struct getpgrp_args *uap;
	int *retval;
{
	register struct proc *p;

	if (uap->pid == 0)
		uap->pid = cp->p_pid;
	p = pfind(uap->pid);
	if (p == 0)
		return (ESRCH);
	if (cp->p_ucred->cr_uid && p->p_ucred->cr_uid != cp->p_ucred->cr_uid &&
	    !inferior(p))
		return (EPERM);
	*retval = p->p_pgid;
	return (0);
}

struct setpgrp_args {
	int	pid;
	int	pgrp;
} *uap;
/*
 * This is the equivalent of BSD setpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid or pgrp.
 */
ultrixsetpgrp(p, uap, retval)
	struct proc *p;
	struct setpgrp_args *uap;
	int *retval;
{
	/* empirically determined */
	if (uap->pgrp < 0 || uap->pgrp >= 30000)
		return (EINVAL);
	return (setpgid(p, uap, retval));
}

struct sigvec_args {
	int	signo;
	struct	sigvec *nsv;
	struct	sigvec *osv;
	caddr_t	sigcode;	/* handler return address */
};

ultrixsigvec(p, uap, retval)
	struct proc *p;
	register struct sigvec_args *uap;
	int *retval;
{
	return (osigvec(p, uap, retval));
}

ultrixsigcleanup(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{
	printf("ultrixsigcleanup %s %d\n", p->p_comm, p->p_pid); /* XXX */
	return (ENOSYS);
}

ultrixsigreturn(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{
	printf("ultrixsigreturn %s %d\n", p->p_comm, p->p_pid); /* XXX */
	return (ENOSYS);
}

/*
 * Switch process from ULTRIX emulation to BSD.
 */
ultrixtobsd(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	p->p_md.md_flags &= ~MDP_ULTRIX;
	return (0);
}

ultrixgetsysinfo(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	/*
	 * Just return a 0.  This says that the requested information is
	 * not available which is certainly true for the most part.
	 */
	retval[0] = 0;
	return (0);
}

#endif
