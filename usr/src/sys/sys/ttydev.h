/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttydev.h	7.4 (Berkeley) %G%
 */

/*
 * Terminal definitions related to underlying hardware.
 */
#ifndef _TTYDEV_
#define	_TTYDEV_

/*
 * Speeds
 */
#ifdef USE_OLD_TTY
/*
 * Speeds
 */
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
#else
#define B0	0
#define B50	50
#define B75	75
#define B110	110
#define B134	134
#define B150	150
#define B200	200
#define B300	300
#define B600	600
#define B1200	1200
#define	B1800	1800
#define B2400	2400
#define B4800	4800
#define B9600	9600
#define B19200	19200
#define B38400	38400
#define EXTA	14
#define EXTB	15
#endif

#ifdef KERNEL

struct speedtab {
	int sp_speed;
	int sp_code;
};

/*
 * Modem control commands.
 */
#define	DMSET		0
#define	DMBIS		1
#define	DMBIC		2
#define	DMGET		3

/*
 * Exceptional conditions possible on character input.
 */
#define TTY_FE		0x01000000	/* Framing error or BREAK condition */
#define TTY_PE		0x02000000	/* Parity error */
#define TTY_CHARMASK	0x000000ff	/* Character mask */
#define TTY_QUOTE	0x00000100	/* Character quoted */
#define TTY_ERRORMASK	0xff000000	/* Error mask */

#endif /* KERNEL */

#endif /* _TTYDEV_ */
