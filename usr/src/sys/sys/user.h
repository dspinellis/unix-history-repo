/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)user.h	7.17 (Berkeley) %G%
 */

#ifndef KERNEL
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/uio.h>
#endif
#include <machine/pcb.h>
#include <sys/signalvar.h>
#include <sys/resourcevar.h>
#include <sys/namei.h>

/*
 * Per process structure containing data that
 * isn't needed in core when the process is swapped out.
 */
 
struct	user {
	struct	pcb u_pcb;
	struct	proc *u_procp;		/* pointer to proc structure XXX */
	label_t	u_ssave;		/* label variable for swapping XXX */
#define curproc	u.u_procp		/* XXX */

	struct	sigacts u_sigacts;	/* p_sigacts points here (use it!) */
	struct	pstats u_stats;		/* rusage, profiling & timers */

/* 1.6 - resource controls */

	int	u_spare[1];

};

/* u_error codes */
#ifndef KERNEL
#include <errno.h>
#endif

#ifdef KERNEL
extern	struct user u;
extern	struct user *swaputl;
extern	struct user *forkutl;
extern	struct user *xswaputl;
extern	struct user *xswap2utl;
extern	struct user *pushutl;
extern	struct user *vfutl;
#endif
