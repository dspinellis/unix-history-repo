/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: trap.h 1.7 91/03/25$
 *
 *	@(#)trap.h	7.6 (Berkeley) %G%
 */

/*
 * Trap codes
 */

#define	T_BUSERR	0
#define	T_ADDRERR	1
#define	T_ILLINST	2
#define	T_ZERODIV	3
#define	T_CHKINST	4
#define	T_TRAPVINST	5
#define	T_PRIVINST	6
#define	T_TRACE		7
#define	T_MMUFLT	8
#define	T_SSIR		9
#define	T_FMTERR	10
#define T_FPERR		11
#define T_COPERR	12
#define T_ASTFLT	13
#define T_TRAP15	15
#define T_FPEMULI	16
#define T_FPEMULD	17

#define	T_USER		0x80		/* user-mode flag or'ed with type */

