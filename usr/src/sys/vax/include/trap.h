/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.h	6.2 (Berkeley) %G%
 */

/*
 * Trap type values
 */

/* The first three constant values are known to the real world <signal.h> */
#define	T_RESADFLT	0		/* reserved addressing fault */
#define	T_PRIVINFLT	1		/* privileged instruction fault */
#define	T_RESOPFLT	2		/* reserved operand fault */
/* End of known constants */
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
