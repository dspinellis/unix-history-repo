/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)clock.c	5.1 (Berkeley) %G%
 */

/*
 * Primitive clock interrupt routines.
 */
#include "param.h"
#include "time.h"
#include "kernel.h"
#include "icu.h"

startrtclock() {

	/* initialize 8253 clock */
	outb (0x43, 0x36);
	outb (0x40, 1193182/60);
	outb (0x40, (1193182/60)/256);
}

clkreld() {
pg("clkreld");
}

/*
 * Initialze the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{

	time.tv_sec = base;
}

/*
 * Restart the clock.
 */
resettodr()
{
}

enablertclock() {
	INTREN(IRQ0);
	splnone();
}
