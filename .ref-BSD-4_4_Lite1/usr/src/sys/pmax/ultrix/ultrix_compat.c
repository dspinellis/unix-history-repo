/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: Utah $Hdr: hpux_compat.c 1.41 91/04/06$
 *
 *	@(#)ultrix_compat.c	8.1 (Berkeley) 6/10/93
 */

/*
 * Various ULTRIX compatibility routines
 */

#ifdef ULTRIXCOMPAT

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/signalvar.h>
#include <sys/kernel.h>
#include <sys/filedesc.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/ioctl.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/syslog.h>
#include <sys/malloc.h>
#include <sys/mount.h>
#include <sys/ipc.h>
#include <sys/user.h>

#include <machine/cpu.h>
#include <machine/reg.h>
#include <machine/psl.h>
#include <machine/vmparam.h>

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
