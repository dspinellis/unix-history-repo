/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.h	7.1 (Berkeley) %G%
 */

/*
 * Trap type values
 * also known in trap.c for name strings
 */

#define	T_RESADFLT	0	/* reserved addressing */
#define	T_PRIVINFLT	1	/* privileged instruction */
#define	T_RESOPFLT	2	/* reserved operand */
#define	T_BPTFLT	3	/* breakpoint instruction */
#define	T_SYSCALL	5	/* system call (kcall) */
#define	T_ARITHTRAP	6	/* arithmetic trap */
#define	T_ASTFLT	7	/* system forced exception */
#define	T_SEGFLT	8	/* segmentation (limit) fault */
#define	T_PROTFLT	9	/* protection fault */
#define	T_TRCTRAP	10	/* trace trap */
#define	T_PAGEFLT	12	/* page fault */
#define	T_TABLEFLT	13	/* page table fault */
#define	T_ALIGNFLT	14	/* alignment fault */
#define	T_KSPNOTVAL	15	/* kernel stack pointer not valid */
#define	T_BUSERR	16	/* bus error */
#define	T_KDBTRAP	17	/* kernel debugger trap */

/* definitions for <sys/signal.h> */
#define	    ILL_RESAD_FAULT	T_RESADFLT
#define	    ILL_PRIVIN_FAULT	T_PRIVINFLT
#define	    ILL_RESOP_FAULT	T_RESOPFLT
#define	    ILL_ALIGN_FAULT	T_ALIGNFLT

/* codes for SIGFPE/ARITHTRAP */
#define	    FPE_INTOVF_TRAP	0x1	/* integer overflow */
#define	    FPE_INTDIV_TRAP	0x2	/* integer divide by zero */
#define	    FPE_FLTDIV_TRAP	0x3	/* floating/decimal divide by zero */
#define	    FPE_FLTOVF_TRAP	0x4	/* floating overflow */
#define	    FPE_FLTUND_TRAP	0x5	/* floating underflow */
