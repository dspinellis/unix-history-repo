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
 *	@(#)clockreg.h	7.2 (Berkeley) %G%
 */

#define	SECMIN	((unsigned)60)			/* seconds per minute */
#define	SECHOUR	((unsigned)(60*SECMIN))		/* seconds per hour */
#define	SECDAY	((unsigned)(24*SECHOUR))	/* seconds per day */
#define	SECYR	((unsigned)(365*SECDAY))	/* seconds per common year */

#define	YRREF		1970
#define	LEAPYEAR(year)	(((year) % 4) == 0)

#define MK48T02

#define SET_CLOCK       0x80
#define READ_CLOCK      0x40
