/*-
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ttydev.h	8.1 (Berkeley) %G%
 */

/* COMPATABILITY HEADER FILE */

#ifndef _TTYDEV_H_
#define	_TTYDEV_H_

#ifdef USE_OLD_TTY
#define B0	0
#define B50	1
#define B75	2
#define B110	3
#define B134	4
#define B150	5
#define B200	6
#define B300	7
#define B600	8
#define B1200	9
#define	B1800	10
#define B2400	11
#define B4800	12
#define B9600	13
#define EXTA	14
#define EXTB	15
#endif /* USE_OLD_TTY */

#endif /* !_TTYDEV_H_ */
