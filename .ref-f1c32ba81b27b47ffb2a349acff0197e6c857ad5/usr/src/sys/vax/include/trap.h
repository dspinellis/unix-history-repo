/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.h	7.3 (Berkeley) %G%
 */

/*
 * Trap type values
 * also known in trap.c for name strings
 */

#define	T_RESADFLT	0		/* reserved addressing fault */
#define	T_PRIVINFLT	1		/* privileged instruction fault */
#define	T_RESOPFLT	2		/* reserved operand fault */
/* definitions for <sys/signal.h> */
#define	    ILL_RESAD_FAULT	T_RESADFLT
#define	    ILL_PRIVIN_FAULT	T_PRIVINFLT
#define	    ILL_RESOP_FAULT	T_RESOPFLT
/* CHME, CHMS, CHMU are not yet given back to users reasonably */
#define	T_BPTFLT	3		/* bpt instruction fault */
#define	T_XFCFLT	4		/* xfc instruction fault */
#define	T_SYSCALL	5		/* chmk instruction (syscall trap) */
#define	T_ARITHTRAP	6		/* arithmetic trap */
#define	T_ASTFLT	7		/* software level 2 trap (ast deliv) */
#define	T_SEGFLT	8		/* segmentation fault */
#define	T_PROTFLT	9		/* protection fault */
#define	T_TRCTRAP	10		/* trace trap */
#define	T_COMPATFLT	11		/* compatibility mode fault */
#define	T_PAGEFLT	12		/* page fault */
#define	T_TABLEFLT	13		/* page table fault */
#define	T_KDBTRAP	14		/* kernel debugger trap */

/* codes for SIGFPE/ARITHTRAP */
#define	    FPE_INTOVF_TRAP	0x1	/* integer overflow */
#define	    FPE_INTDIV_TRAP	0x2	/* integer divide by zero */
#define	    FPE_FLTOVF_TRAP	0x3	/* floating overflow */
#define	    FPE_FLTDIV_TRAP	0x4	/* floating/decimal divide by zero */
#define	    FPE_FLTUND_TRAP	0x5	/* floating underflow */
#define	    FPE_DECOVF_TRAP	0x6	/* decimal overflow */
#define	    FPE_SUBRNG_TRAP	0x7	/* subscript out of range */
#define	    FPE_FLTOVF_FAULT	0x8	/* floating overflow fault */
#define	    FPE_FLTDIV_FAULT	0x9	/* divide by zero floating fault */
#define	    FPE_FLTUND_FAULT	0xa	/* floating underflow fault */
