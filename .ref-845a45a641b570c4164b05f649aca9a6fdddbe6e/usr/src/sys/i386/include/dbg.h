/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dbg.h	7.1 (Berkeley) %G%
 */

/*
 * Screen debug flags
 */
#define	DPAUSE		0x0001	/* wait for key press */
#define	DALLTRAPS	0x0002	/* print on alltraps */
#define	DALLSYSC	0x0004	/* print on allsystem calls */
#define	DSYSFAIL	0x0008	/* print on system call failures */
#define	DPAGIN		0x0010	/* print on pagin activity */
#define	DEXEC		0x0020	/* print on exec activity */
#define	DNAMEI		0x0040	/* print on namei activity */
#define	DEXPAND		0x0080	/* print on segment expand activity */
#define	DCLK		0x0100	/* print on clock activity */
#define	DDSK		0x0200	/* print on disk activity */
#define	DSIGNAL		0x0400	/* print on signal delivery */
