/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trap.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <machine/frame.h>
#include <luna68k/stand/romvec.h>

#define	USER	040		/* user-mode flag added to type */

char	*trap_type[] = {
	"Bus error",
	"Address error",
	"Illegal instruction",
	"Zero divide",
	"CHK instruction",
	"TRAPV instruction",
	"Privilege violation",
	"Trace trap",
	"MMU fault",
	"SSIR trap",
	"Format error",
	"68881 exception",
	"Coprocessor violation",
	"Async system trap"
};
#define	TRAP_TYPES	(sizeof trap_type / sizeof trap_type[0])

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(type, code, v, frame)
	int type;
	unsigned code;
	register unsigned v;
	struct frame frame;
{
	switch (type) {

	default:
dopanic:
		printf("trap type %d, code = %x, v = %x\n", type, code, v);
		regdump(frame.f_regs, 128);
		type &= ~USER;
		if ((unsigned)type < TRAP_TYPES)
			panic(trap_type[type]);
		panic("trap");
	}
}
