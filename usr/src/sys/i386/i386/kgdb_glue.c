/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kgdb_glue.c	7.3 (Berkeley) %G%
 */

/*
 * This file must be compiled with gcc -fno-defer-pop.
 */

#ifdef KGDB

#include <sys/param.h>
#include <machine/frame.h>
#include <machine/reg.h>

#ifndef lint
static char rcsid[] =
    "@(#) $Header: /u/donn/c/gdb/kernel/RCS/kgdb_glue.c,v 1.2 91/03/31 16:04:52 donn Exp Locker: donn $ (LBL)";
#endif

#define KGDB_STACKSIZE 0x800
#define KGDB_STACKWORDS (KGDB_STACKSIZE / sizeof(u_long))

u_long kgdb_stack[KGDB_STACKWORDS];

#define getsp(v) asm volatile ("movl %%esp,%0" : "=r" (v))
#define setsp(v) asm volatile ("movl %0,%%esp" :: "r" (v))

static inline void
copywords(src, dst, nbytes)
	register u_long *src, *dst;
	register u_int nbytes;
{
	u_long *limit = src + (nbytes / sizeof(u_long));

	do {
		*dst++ = *src++;
	} while (src < limit);
	if (nbytes & 2)
		*(u_short *)dst = *(u_short *)src;
}

kgdb_trap_glue(frame)
	struct trapframe frame;
{
	u_long osp, nsp;
	u_int s;

	/*
	 * On internal traps, the hardware doesn't push ss and esp.
	 */
	u_int fsize = sizeof frame - 2 * sizeof (int);
#ifdef DONN
	extern int kgdb_debug;

	if (kgdb_debug > 3)
		pg("kgdb_trap_glue: entered, fsize = %d\n", fsize);
#endif

	/*
	 * After a kernel mode trap, the saved sp doesn't point to the right
	 * place.  The correct value is the top of the frame.
	 */
	frame.tf_isp = (u_long)(&frame) + fsize;

	/*
	 * Copy the interrupt context and frame to the new stack.
	 * We're throwing away trap()'s frame since we're going to do
	 * our own iret.
	 */
	nsp = (u_long)(&kgdb_stack[KGDB_STACKWORDS]) - fsize;

	copywords((u_long *)&frame, (u_long *)nsp, fsize);

	s = splhigh();

	getsp(osp);
	setsp(nsp);

	if (kgdb_trap(frame.tf_trapno, (struct frame *)nsp) == 0) {
		/*
		 * Get back on kernel stack.  This thread of control
		 * will return back up through trap().  If kgdb_trap()
		 * returns 0, it didn't handle the trap at all so
		 * the stack is still intact and everything will
		 * unwind okay from here up.
		 */
		setsp(osp);
		splx(s);
		return 0;
	}

	/*
	 * Copy back context, which has possibly changed.  Even the
	 * sp might have changed.
	 */
	osp = ((struct trapframe *)nsp)->tf_isp - fsize;
	copywords((u_long *)nsp, (u_long *)osp, fsize);
	setsp(osp);

	/*
	 * Restore the possible new context from frame.
	 */
	asm volatile ("pop %es; pop %ds; popal; nop; addl $8,%esp; iret");
}

int kgdb_testval;

kgdb_test(i)
{
        ++kgdb_testval;
        return i + 1;
}

#endif
