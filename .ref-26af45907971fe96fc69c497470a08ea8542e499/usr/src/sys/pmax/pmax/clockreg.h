/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clockreg.h 1.14 91/01/18$
 *
 *	@(#)clockreg.h	7.1 (Berkeley) %G%
 */

/*
 * This file contains definitions for the MC 146818 real-time clock.
 *
 * For a detailed explanation of the chip, see the "PMAX Desktop
 * Workstation Functional Specification, Revision 1.1" pages 62-66.
 */
#define	SECMIN	((unsigned)60)			/* seconds per minute */
#define	SECHOUR	((unsigned)(60*SECMIN))		/* seconds per hour */
#define	SECDAY	((unsigned)(24*SECHOUR))	/* seconds per day */
#define	SECYR	((unsigned)(365*SECDAY))	/* seconds per common year */

#define	YRREF		1970
#define	LEAPYEAR(year)	(((year) % 4) == 0)

/*
 * Definitions for MC146818 real time clock
 */
struct chiptime {
	u_char	sec;		/* current seconds */
	char	dummy0[3];
	u_char	alarm_sec;	/* alarm seconds */
	char	dummy1[3];
	u_char	min;		/* current minutes */
	char	dummy2[3];
	u_char	alarm_min;	/* alarm minutes */
	char	dummy3[3];
	u_char	hour;		/* current hours */
	char	dummy4[3];
	u_char	alarm_hour;	/* alarm hours */
	char	dummy5[3];
	u_char	dayw;		/* day of the week */
	char	dummy6[3];
	u_char	day;		/* day of the month */
	char	dummy7[3];
	u_char	mon;		/* month */
	char	dummy8[3];
	u_char	year;		/* year */
	char	dummy9[3];
	u_char	rega;		/* register a */
	char	dummy10[3];
	u_char	regb;		/* register b */
	char	dummy11[3];
	u_char	regc;		/* register c */
	char	dummy12[3];
	u_char	regd;		/* register d */
	char	dummy13[3];
	u_char	nvram[50*4];	/* battery backed-up ram */
};

/*
 * Control register A fields.
 */
#define REGA_UIP		0x80
#define REGA_TIME_DIV		0x70
#define REGA_RATE_SELECT	0x0F

/*
 * Time base to use in the REGA_TIME_DIV field.
 */
#define REGA_TIME_BASE		0x20

/*
 * Set the interval at 15.625 ms.
 */
#define SELECTED_RATE		0xA

/*
 * Control register B fields.
 */
#define REGB_SET_TIME		0x80
#define REGB_PER_INT_ENA	0x40
#define REGB_UPDATE_INT_ENA	0x10
#define REGB_DATA_MODE		0x04
#define REGB_HOURS_FORMAT	0x02

/*
 * Control register C fields.
 */
#define REGC_INT_PENDING	0x80
#define REGC_PER_INT_PENDING	0x40
#define REGC_UPDATE_INT_PENDING	0x10

/*
 * Control register D fields.
 */
#define REGD_VALID_TIME		0x80

/*
 * The RTC registers can only be accessed one byte at a time.
 * This routine is used to write words into the non-volatile storage.
 */

#define BYTECOPY(a,b,num) { \
	int i; \
	for (i = 0; i < (num); i++) \
		((char *) (b))[i] = ((char *) (a))[i]; \
}
