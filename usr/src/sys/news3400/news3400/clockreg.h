/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department, Ralph Campbell, Sony Corp. and Kazumasa
 * Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clockreg.h 1.14 91/01/18$
 *
 *	@(#)clockreg.h	7.1 (Berkeley) %G%
 */

#define	SECMIN	((unsigned)60)			/* seconds per minute */
#define	SECHOUR	((unsigned)(60*SECMIN))		/* seconds per hour */
#define	SECDAY	((unsigned)(24*SECHOUR))	/* seconds per day */
#define	SECYR	((unsigned)(365*SECDAY))	/* seconds per common year */

#define	YRREF		1970
#define	LEAPYEAR(year)	(((year) % 4) == 0)

/*
 * Definitions for real time clock
 */
struct chiptime {
	u_char	sec;		/* current seconds */
	u_char	min;		/* current minutes */
	u_char	hour;		/* current hours */
	u_char	dayw;		/* day of the week */
	u_char	day;		/* day of the month */
	u_char	mon;		/* month */
	u_char	year;		/* year */
};

#if defined(CPU_SINGLE) && !defined(news700)

#define MK48T02

#define SET_CLOCK       0x80
#define READ_CLOCK      0x40

#endif /* CPU_SINGLE && !news700 */
