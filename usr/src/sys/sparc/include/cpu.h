/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)cpu.h	8.4 (Berkeley) 1/5/94
 *
 * from: $Header: cpu.h,v 1.12 93/05/25 10:36:34 torek Exp $ (LBL)
 */

#ifndef _CPU_H_
#define _CPU_H_

/*
 * CTL_MACHDEP definitinos.
 */
#define	CPU_MAXID	1	/* no valid machdep ids */

#define	CTL_MACHDEP_NAMES { \
	{ 0, 0 }, \
}

#ifdef KERNEL
/*
 * Exported definitions unique to SPARC cpu support.
 */

#include <machine/psl.h>
#include <sparc/sparc/intreg.h>

/*
 * definitions of cpu-dependent requirements
 * referenced in generic code
 */
#define	COPY_SIGCODE		/* copy sigcode above user stack in exec */

#define	cpu_exec(p)		/* nothing */
#define	cpu_swapin(p)		/* nothing */
#define	cpu_wait(p)		/* nothing */
#define	cpu_setstack(p, ap)	((p)->p_md.md_tf->tf_out[6] = (ap) - 64)

/*
 * See syscall() for an explanation of the following.  Note that the
 * locore bootstrap code follows the syscall stack protocol.  The
 * framep argument is unused.
 */
#define cpu_set_init_frame(p, fp) \
	(p)->p_md.md_tf = (struct trapframe *) \
	    ((caddr_t)(p)->p_addr + UPAGES * NBPG - sizeof(struct trapframe))

/*
 * Arguments to hardclock, softclock and gatherstats encapsulate the
 * previous machine state in an opaque clockframe.  The ipl is here
 * as well for strayintr (see locore.s:interrupt and intr.c:strayintr).
 * Note that CLKF_INTR is valid only if CLKF_USERMODE is false.
 */
struct clockframe {
	u_int	psr;		/* psr before interrupt, excluding PSR_ET */
	u_int	pc;		/* pc at interrupt */
	u_int	npc;		/* npc at interrupt */
	u_int	ipl;		/* actual interrupt priority level */
	u_int	fp;		/* %fp at interrupt */
};

extern int eintstack[];

#define	CLKF_USERMODE(framep)	(((framep)->psr & PSR_PS) == 0)
#define	CLKF_BASEPRI(framep)	(((framep)->psr & PSR_PIL) == 0)
#define	CLKF_PC(framep)		((framep)->pc)
#define	CLKF_INTR(framep)	((framep)->fp < (u_int)eintstack)

/*
 * Software interrupt request `register'.
 */
union sir {
	int	sir_any;
	char	sir_which[4];
} sir;

#define SIR_NET		0
#define SIR_CLOCK	1

#define	setsoftint()	ienab_bis(IE_L1)
#define setsoftnet()	(sir.sir_which[SIR_NET] = 1, setsoftint())
#define setsoftclock()	(sir.sir_which[SIR_CLOCK] = 1, setsoftint())

int	want_ast;

/*
 * Preempt the current process if in interrupt from user mode,
 * or after the current trap/syscall if in system mode.
 */
int	want_resched;		/* resched() was called */
#define	need_resched()		(want_resched = 1, want_ast = 1)

/*
 * Give a profiling tick to the current process when the user profiling
 * buffer pages are invalid.  On the sparc, request an ast to send us 
 * through trap(), marking the proc as needing a profiling tick.
 */
#define	need_proftick(p)	((p)->p_flag |= P_OWEUPC, want_ast = 1)

/*
 * Notify the current process (p) that it has a signal pending,
 * process as soon as possible.
 */
#define	signotify(p)		(want_ast = 1)

/*
 * Only one process may own the FPU state.
 *
 * XXX this must be per-cpu (eventually)
 */
struct	proc *fpproc;		/* FPU owner */
int	foundfpu;		/* true => we have an FPU */

/*
 * Interrupt handler chains.  Interrupt handlers should return 0 for
 * ``not me'' or 1 (``I took care of it'').  intr_establish() inserts a
 * handler into the list.  The handler is called with its (single)
 * argument, or with a pointer to a clockframe if ih_arg is NULL.
 */
struct intrhand {
	int	(*ih_fun) __P((void *));
	void	*ih_arg;
	struct	intrhand *ih_next;
} *intrhand[15];

void	intr_establish __P((int level, struct intrhand *));

/*
 * intr_fasttrap() is a lot like intr_establish, but is used for ``fast''
 * interrupt vectors (vectors that are not shared and are handled in the
 * trap window).  Such functions must be written in assembly.
 */
void	intr_fasttrap __P((int level, void (*vec)(void)));

#endif /* KERNEL */
#endif /* _CPU_H_ */
