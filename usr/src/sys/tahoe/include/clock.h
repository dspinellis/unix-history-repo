/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)clock.h	7.1 (Berkeley) %G%
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
