/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)clock.h	7.3 (Berkeley) %G%
 */

#define	SECDAY		((unsigned)(24*60*60))		/* seconds per day */
#define	SECYR		((unsigned)(365*SECDAY))	/* per common year */

#define	YRREF		1970
#define	LEAPYEAR(year)	((year)%4==0)	/* good till time becomes negative */

/*
 * Software clock is software interrupt level 8
 */
#define	setsoftclock()	mtpr(SIRR, 0x8)

/*
 * To calculate value for interval timer register, we
 * use the fact that 20512 yields a 60hz clock.
 */
#define	hztocount(v)	((20512*60) / (v))
