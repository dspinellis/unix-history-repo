/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
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
    "@(#) $Header: kgdb_glue.c,v 1.4 91/03/08 07:02:37 van Exp $ (LBL)";
#endif

/*
 * # of additional bytes in 680x0 exception frame format n.
 */
static int frame_bytes[16] = {
	0, 0, sizeof(struct fmt2), 0,
	0, 0, 0, 0,
	0, sizeof(struct fmt9), sizeof(struct fmtA), sizeof(struct fmtB),
	0, 0, 0, 0
};

#define KGDB_STACKSIZE 0x800
#define KGDB_STACKWORDS (KGDB_STACKSIZE / sizeof(u_long))

u_long kgdb_stack[KGDB_STACKWORDS];

#define getsp(v) asm("movl sp, %0" : "=r" (v))
#define setsp(v) asm("movl %0, sp" :: "r" (v))

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

kgdb_trap_glue(type, frame)
	int type;
	struct frame frame;
{
	u_long osp, nsp;
	u_int fsize, s;

	/*
	 * After a kernel mode trap, the saved sp doesn't point to the right
	 * place.  The correct value is the top of the frame.
	 */
	fsize = sizeof(frame) - sizeof(frame.F_u) + frame_bytes[frame.f_format];
	frame.f_regs[SP] = (u_long)&frame + fsize;

	/*
	 * Copy the interrupt context and frame to the new stack.
	 * We're throwing away trap()'s frame since we're going to do
	 * our own rte.
	 */
	nsp = (u_long)&kgdb_stack[KGDB_STACKWORDS] -
	      roundup(fsize, sizeof(u_long));

	copywords((u_long *)&frame, (u_long *)nsp, fsize);

	s = splhigh();

	getsp(osp);
	setsp(nsp);

	if (kgdb_trap(type, (struct frame *)nsp) == 0) {
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
	osp = ((struct frame *)nsp)->f_regs[SP] - fsize;
	copywords((u_long *)nsp, (u_long *)osp, fsize);
	setsp(osp);

	/*
	 * Restore the possible new context from frame, pop the
	 * unneeded usp (we trapped from kernel mode) and pad word,
	 * and return to the trapped thread.
	 */
	asm("moveml sp@+,#0x7FFF; addql #6,sp; rte");
}

int kgdb_testval;

kgdb_test(i)
{
        ++kgdb_testval;
        return i + 1;
}

#endif
