/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Areadable.c	7.1 (Berkeley) %G%
 */

#include "align.h" 

long readable(infop, address, length)
process_info 	*infop;
long		address, length;
/*
 *   Return TRUE (= -1) if the specified bytes can be read without an access
 * control violation (limit and/or protection). Page faults are OK.
 *   If problems, return the code that would be pushed by HW on the
 * stack (see the architecture manual).
 *   Assumption is that in most cases, access is OK, so a quick 'prober'
 * will be enough. If not, we have to work harder to determine the exact
 * cause and return the right code, without getting the fault here in
 * the kernel !!.
 *
 * The address is assumed to be read for the user.!
 */
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;
	register	long	Register_10;
	register	long	Register_9;
	register	long	Register_8;
	register	long	subspace;
	register	long	last_page;

	Register_12 = address;
	Register_11 = length-1;
	asm ("		prober	$1,(r12),$1	");	/* Yeach ... */
	asm ("		beql	no_access	");
	asm ("		addl2	r11,r12		");	/* last byte */
	asm ("		prober	$1,(r12),$1	");
	asm ("		beql	no_access	");
	asm ("		movl	$-1,r0		");	/* TRUE */
	asm ("		ret#1			");
	asm ("no_access:			");
/*
 * Now the hard work. Have to check length violation first.
 * If any byte (first or last) causes a length violation, report it as such.
 */
	asm ("	mfpr	$3,r8	");	/* Get length registers. P0LR */
	asm ("	mfpr	$5,r9	");	/* P1LR */
	asm ("	mfpr	$7,r10	");	/* P2LR */
	asm ("	mfpr	$1,r11	");	/* SLR  */

	subspace = (address >> 30) & 3;
	Register_12 = (address >> 10) & 0xfffff;	/* 1'st byte page # */
	last_page = ( (address+length-1) >> 10) & 0xfffff;
	switch ( subspace ) {
	case 0:
		if ( (Register_12 >= Register_8) ||
		     (last_page   >= Register_8) ) return (1);
		break;
	case 1:
		if ( (Register_12 >= Register_9) ||
		     (last_page   >= Register_9) ) return (1);
		break;
	case 2:
		if ( (Register_12 < Register_10) ||
		     (last_page   < Register_10) ) return (1);
		break;
	case 3:
		if ( (Register_12 >= Register_11) ||
		     (last_page   >= Register_11) ) return (1);
		break;
	}
/*
 * OK, it's not a length violation. Must have been an access problem
 * (no read by user).
 *
 * NOTE : I definitely ignore the case of 'no PTE access' since I
 *	assume that's not the case for user mode. Besides, the poor
 *	guy will just get an access violation that will most probably
 *	send him into hyperspace anyway, so no need to be too acurate here.
 */
	return (0);
}
