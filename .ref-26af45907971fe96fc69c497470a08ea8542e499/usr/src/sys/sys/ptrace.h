/*-
 * Copyright (c) 1984, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ptrace.h	8.1 (Berkeley) %G%
 */

#ifndef	_PTRACE_H_
#define	_PTRACE_H_

#define	PT_TRACE_ME	0	/* child declares it's being traced */
#define	PT_READ_I	1	/* read word in child's I space */
#define	PT_READ_D	2	/* read word in child's D space */
#define	PT_READ_U	3	/* read word in child's user structure */
#define	PT_WRITE_I	4	/* write word in child's I space */
#define	PT_WRITE_D	5	/* write word in child's D space */
#define	PT_WRITE_U	6	/* write word in child's user structure */
#define	PT_CONTINUE	7	/* continue the child */
#define	PT_KILL		8	/* kill the child process */
#define	PT_STEP		9	/* single step the child */
#define	PT_ATTACH	10	/* trace some running process */
#define	PT_DETACH	11	/* stop tracing a process */

#define	PT_FIRSTMACH	32	/* for machine-specific requests */
#include <machine/ptrace.h>	/* machine-specific requests, if any */

#ifdef KERNEL
void	proc_reparent __P((struct proc *child, struct proc *newparent));
#else /* !KERNEL */

#include <sys/cdefs.h>

__BEGIN_DECLS
int	ptrace __P((int _request, pid_t _pid, caddr_t _addr, int _data));
__END_DECLS

#endif /* !KERNEL */

#endif	/* !_PTRACE_H_ */
