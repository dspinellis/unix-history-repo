/*	clock.h	1.3	86/12/06	*/

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
