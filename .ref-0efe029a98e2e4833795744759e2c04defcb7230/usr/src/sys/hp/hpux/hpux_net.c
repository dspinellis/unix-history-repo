/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_net.c 1.33 89/08/23$
 *
 *	@(#)hpux_net.c	7.6 (Berkeley) %G%
 */

/*
 * Network related HP-UX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/kernel.h"
#include "sys/time.h"
#include "sys/errno.h"
#include "sys/proc.h"
#include "sys/file.h"
#include "sys/mbuf.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "sys/uio.h"
#include "sys/ktrace.h"
#include "hpux.h"

#define MINBSDIPCCODE	0x3EE
#define NUMBSDIPC	32

/*
 * HPUX netioctl() to BSD syscall map.
 * Indexed by callno - MINBSDIPCCODE
 */
extern int socket(), listen(), bind(), oaccept(), connect(), orecv();
extern int osend(), shutdown(), ogetsockname(), sendto();
extern int orecvfrom(), ogetpeername();
int hpuxgetsockopt(), hpuxsetsockopt();
struct file *getsock();

struct hpuxtobsdipc {
	int (*rout)();
	int nargs;
} hpuxtobsdipc[NUMBSDIPC] = {
	socket,		3, /* 3ee */	listen,		2, /* 3ef */
	bind,		3, /* 3f0 */	oaccept,	3, /* 3f1 */
	connect,	3, /* 3f2 */	orecv,		4, /* 3f3 */
	osend,		4, /* 3f4 */	shutdown,	2, /* 3f5 */
	ogetsockname,	3, /* 3f6 */	hpuxsetsockopt,	5, /* 3f7 */
	sendto,		6, /* 3f8 */	orecvfrom,	6, /* 3f9 */
	ogetpeername,	3, /* 3fa */	NULL,		0, /* 3fb */
	NULL,		0, /* 3fc */	NULL,		0, /* 3fd */
	NULL,		0, /* 3fe */	NULL,		0, /* 3ff */
	NULL,		0, /* 400 */	NULL,		0, /* 401 */
	NULL,		0, /* 402 */	NULL,		0, /* 403 */
	NULL,		0, /* 404 */	NULL,		0, /* 405 */
	NULL,		0, /* 406 */	NULL,		0, /* 407 */
	NULL,		0, /* 408 */	NULL,		0, /* 409 */
	NULL,		0, /* 40a */	hpuxgetsockopt,	5, /* 40b */
	NULL,		0, /* 40c */	NULL,		0, /* 40d */
};

/*
 * Single system call entry to BSD style IPC.
 * Gleened from disassembled libbsdipc.a syscall entries.
 */
hpuxnetioctl(p, uap, retval)
	struct proc *p;
	struct args {
		int	call;
		int	*args;
	} *uap;
	int *retval;
{
	int *args, i;
	register int code;
	int error;

	args = uap->args;
	code = uap->call - MINBSDIPCCODE;
	if (code < 0 || code >= NUMBSDIPC || hpuxtobsdipc[code].rout == NULL)
		return (EINVAL);
	if ((i = hpuxtobsdipc[code].nargs * sizeof (int)) &&
	    (error = copyin((caddr_t)args, (caddr_t)uap, (u_int)i))) {
#ifdef KTRACE
                if (KTRPOINT(p, KTR_SYSCALL))
                        ktrsyscall(p->p_tracep, code + MINBSDIPCCODE,
				   hpuxtobsdipc[code].nargs);
#endif
		return (error);
	}
#ifdef KTRACE
        if (KTRPOINT(p, KTR_SYSCALL))
                ktrsyscall(p->p_tracep, code + MINBSDIPCCODE,
			   hpuxtobsdipc[code].nargs);
#endif
	return ((*hpuxtobsdipc[code].rout)(p, uap, retval));
}

hpuxsetsockopt(p, uap, retval)
	struct proc *p;
	struct args {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	valsize;
	} *uap;
	int *retval;
{
	struct file *fp;
	struct mbuf *m = NULL;
	int tmp, error;

	fp = getsock(uap->s, &error);
	if (fp == 0)
		return (error);
	if (uap->valsize > MLEN)
		return (EINVAL);
	if (uap->val) {
		m = m_get(M_WAIT, MT_SOOPTS);
		if (m == NULL)
			return (ENOBUFS);
		if (error = copyin(uap->val, mtod(m, caddr_t),
		    (u_int)uap->valsize)) {
			(void) m_free(m);
			return (error);
		}
		if (uap->name == SO_LINGER) {
			tmp = *mtod(m, int *);
			mtod(m, struct linger *)->l_onoff = 1;
			mtod(m, struct linger *)->l_linger = tmp;
			m->m_len = sizeof(struct linger);
		} else
			m->m_len = uap->valsize;
	} else if (uap->name == ~SO_LINGER) {
		m = m_get(M_WAIT, MT_SOOPTS);
		if (m) {
			uap->name = SO_LINGER;
			mtod(m, struct linger *)->l_onoff = 0;
			m->m_len = sizeof(struct linger);
		}
	}
	return (sosetopt((struct socket *)fp->f_data, uap->level,
	    uap->name, m));
}

hpuxgetsockopt(p, uap, retval)
	struct proc *p;
	struct args {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	*avalsize;
	} *uap;
	int *retval;
{
	struct file *fp;
	struct mbuf *m = NULL;
	int valsize, error;

	fp = getsock(uap->s, &error);
	if (fp == 0)
		return (error);
	if (uap->val) {
		if (error = copyin((caddr_t)uap->avalsize, (caddr_t)&valsize,
		    sizeof (valsize)))
			return (error);
	} else
		valsize = 0;
	if (error = sogetopt((struct socket *)fp->f_data, uap->level,
	    uap->name, &m))
		goto bad;
	if (uap->val && valsize && m != NULL) {
		if (uap->name == SO_LINGER) {
			if (mtod(m, struct linger *)->l_onoff)
				*mtod(m, int *) = mtod(m, struct linger *)->l_linger;
			else
				*mtod(m, int *) = 0;
			m->m_len = sizeof(int);
		}
		if (valsize > m->m_len)
			valsize = m->m_len;
		error = copyout(mtod(m, caddr_t), uap->val, (u_int)valsize);
		if (error == 0)
			error = copyout((caddr_t)&valsize,
			    (caddr_t)uap->avalsize, sizeof (valsize));
	}
bad:
	if (m != NULL)
		(void) m_free(m);
	return (error);
}
#endif
