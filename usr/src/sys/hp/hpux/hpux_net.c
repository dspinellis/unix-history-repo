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
 *	@(#)hpux_net.c	7.1 (Berkeley) %G%
 */

/*
 * Network related HP-UX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "file.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "ktrace.h"
#include "hpux.h"

#define MINBSDIPCCODE	0x3EE
#define NUMBSDIPC	32

/*
 * HPUX netioctl() to BSD syscall map.
 * Indexed by callno - MINBSDIPCCODE
 */
extern int socket(), listen(), bind(), accept(), connect(), orecv();
extern int osend(), shutdown(), getsockname(), sendto();
extern int recvfrom(), getpeername();
int hpuxgetsockopt(), hpuxsetsockopt();
struct file *getsock();

struct hpuxtobsdipc {
	int (*rout)();
	int nargs;
} hpuxtobsdipc[NUMBSDIPC] = {
	socket,		3, /* 3ee */	listen,		2, /* 3ef */
	bind,		3, /* 3f0 */	accept,		3, /* 3f1 */
	connect,	3, /* 3f2 */	orecv,		4, /* 3f3 */
	osend,		4, /* 3f4 */	shutdown,	2, /* 3f5 */
	getsockname,	3, /* 3f6 */	hpuxsetsockopt,	5, /* 3f7 */
	sendto,		6, /* 3f8 */	recvfrom,	6, /* 3f9 */
	getpeername,	3, /* 3fa */	NULL,		0, /* 3fb */
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
hpuxnetioctl()
{
	struct a {
		int	call;
		int	*args;
	} *uap = (struct a *)u.u_ap;
	int *args, i;
	register int code;

	args = uap->args;
	code = uap->call - MINBSDIPCCODE;
	if (code < 0 || code >= NUMBSDIPC || hpuxtobsdipc[code].rout == NULL) {
		u.u_error = EINVAL;
		return;
	}
	if ((i = hpuxtobsdipc[code].nargs * sizeof (int)) &&
	    (u.u_error = copyin((caddr_t)args, (caddr_t)u.u_arg, (u_int)i))) {
#ifdef KTRACE
                if (KTRPOINT(u.u_procp, KTR_SYSCALL))
                        ktrsyscall(u.u_procp->p_tracep, code + MINBSDIPCCODE,
				   hpuxtobsdipc[code].nargs);
#endif
		return;
	}
#ifdef KTRACE
        if (KTRPOINT(u.u_procp, KTR_SYSCALL))
                ktrsyscall(u.u_procp->p_tracep, code + MINBSDIPCCODE,
			   hpuxtobsdipc[code].nargs);
#endif
	(*hpuxtobsdipc[code].rout)();
}

hpuxsetsockopt()
{
	struct a {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	valsize;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct mbuf *m = NULL;
	int tmp;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	if (uap->valsize > MLEN) {
		u.u_error = EINVAL;
		return;
	}
	if (uap->val) {
		m = m_get(M_WAIT, MT_SOOPTS);
		if (m == NULL) {
			u.u_error = ENOBUFS;
			return;
		}
		u.u_error =
		    copyin(uap->val, mtod(m, caddr_t), (u_int)uap->valsize);
		if (u.u_error) {
			(void) m_free(m);
			return;
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
	u.u_error =
	    sosetopt((struct socket *)fp->f_data, uap->level, uap->name, m);
}

hpuxgetsockopt()
{
	struct a {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	*avalsize;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct mbuf *m = NULL;
	int valsize;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	if (uap->val) {
		u.u_error = copyin((caddr_t)uap->avalsize, (caddr_t)&valsize,
			sizeof (valsize));
		if (u.u_error)
			return;
	} else
		valsize = 0;
	u.u_error =
	    sogetopt((struct socket *)fp->f_data, uap->level, uap->name, &m);
	if (u.u_error)
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
		u.u_error = copyout(mtod(m, caddr_t), uap->val, (u_int)valsize);
		if (u.u_error)
			goto bad;
		u.u_error = copyout((caddr_t)&valsize, (caddr_t)uap->avalsize,
		    sizeof (valsize));
	}
bad:
	if (m != NULL)
		(void) m_free(m);
}
#endif
