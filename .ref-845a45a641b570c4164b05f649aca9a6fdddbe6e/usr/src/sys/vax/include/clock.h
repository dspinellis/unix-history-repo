/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)clock.h	7.3 (Berkeley) %G%
 */

/*
 * VAX clock registers
 */

#define	ICCS_RUN	0x00000001
#define	ICCS_TRANS	0x00000010
#define	ICCS_SS		0x00000020
#define	ICCS_IE		0x00000040
#define	ICCS_INT	0x00000080
#define	ICCS_ERR	0x80000000
	
#define	SECDAY		((unsigned)(24*60*60))		/* seconds per day */
#define	SECYR		((unsigned)(365*SECDAY))	/* per common year */
/*
 * TODRZERO is the what the TODR should contain when the ``year'' begins.
 * The TODR should always contain a number between 0 and SECYR+SECDAY.
 */
#define	TODRZERO	((unsigned)(1<<28))

#define	YRREF		1970
#define	LEAPYEAR(year)	((year)%4==0)	/* good till time becomes negative */

/*
 * Has the time-of-day clock wrapped around?
 */
#define	clkwrap()	(((unsigned)mfpr(TODR) - TODRZERO)/100 > SECYR+SECDAY)

/*
 * Software clock is software interrupt level 8,
 * implemented as mtpr(SIRR, 0x8) in asm.sed.
 */

#ifndef LOCORE
/*
 * 8200s and 630s have a clock chip like those found in digital alarm
 * clocks and watches.  Converting this to and from system times is
 * painful, so we do it in only one place.  The routine chiptotime()
 * converts a chiptime to the right value for time.tv_sec, and
 * timetochip converts time.tv_sec back.
 */
struct chiptime {
	int	sec;
	int	min;
	int	hour;
	int	day;
	int	mon;
	int	year;
};

/*
 * Clock read routine return values.
 */
#define	CLKREAD_OK	0	/* success, time.tv_sec set */
#define	CLKREAD_WARN	1	/* clock appears wrong but time set anyway */
#define	CLKREAD_BAD	2	/* clock is bad, no time available */
#endif
