/*	clock.h	4.7	82/09/12	*/

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
 * Start a 60 HZ clock.
 */
#define	clkstart() {\
	mtpr(NICR, -1000000/hz);	/* 10 milli-seconds */\
	mtpr(ICCS, ICCS_RUN+ICCS_IE+ICCS_TRANS+ICCS_INT+ICCS_ERR);\
}
#define	clkwrap()	(((unsigned)mfpr(TODR) - TODRZERO)/100 > SECYR+SECDAY)

/*
 * Software clock is software interrupt level 8,
 * implemented as mtpr(SIRR, 0x8) in asm.sed.
 */
