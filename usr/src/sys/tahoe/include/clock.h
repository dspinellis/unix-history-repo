/*	clock.h	4.5	81/02/23	*/

#define	SECDAY		((unsigned)(24*60*60))		/* seconds per day */
#define	SECYR		((unsigned)(365*SECDAY))	/* per common year */

#define	YRREF		1970
#define	LEAPYEAR(year)	((year)%4==0)	/* good till time becomes negative */

/*
 * Software clock is software interrupt level 8
 */
#define	setsoftclock()	mtpr(0x8, SIRR)
