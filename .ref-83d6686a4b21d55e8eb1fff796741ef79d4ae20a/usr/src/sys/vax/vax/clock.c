
/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)clock.c	7.4 (Berkeley) %G%
 */

#include "param.h"
#include "time.h"
#include "kernel.h"

#include "mtpr.h"
#include "clock.h"
#include "cpu.h"

/*
 * Machine-dependent clock routines.
 *
 * Startrtclock restarts the real-time clock, which provides
 * hardclock interrupts to kern_clock.c.
 *
 * Inittodr initializes the time of day hardware which provides
 * date functions.  Its primary function is to use some file
 * system information in case the hardare clock lost state.
 *
 * Resettodr restores the time of day hardware after a time change.
 */

/*
 * Start the real-time clock.
 */
startrtclock()
{

	(*cpuops->cpu_clock->clkstartrt)();
}

/*
 * Initialze the time of day register, based on the time base which is, e.g.
 * from a filesystem.  Base provides the time to within six months,
 * and the time of year clock (if any) provides the rest.
 */
inittodr(base)
	time_t base;
{
	long deltat, badbase = 0;

	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system\n");
		/* read the system clock anyway */
		base = 6*SECYR + 186*SECDAY + SECDAY/2;
		badbase = 1;
	}
	switch ((*cpuops->cpu_clock->clkread)(base)) {

	case CLKREAD_BAD:
		/*
		 * Believe the time in the file system for lack of
		 * anything better, resetting the TODR.
		 */
		time.tv_sec = base;
		if (!badbase)
			resettodr();
		break;

	case CLKREAD_WARN:
		break;

	case CLKREAD_OK:
		if (badbase)
			break;
		/*
		 * See if we gained/lost two or more days;
		 * if so, assume something is amiss.
		 */
		deltat = time.tv_sec - base;
		if (deltat < 0)
			deltat = -deltat;
		if (deltat < 2 * SECDAY)
			return;
		printf("WARNING: clock %s %d days",
		    time.tv_sec < base ? "lost" : "gained", deltat / SECDAY);
		break;

	default:
		panic("inittodr");
		/* NOTREACHED */
	}
	printf(" -- CHECK AND RESET THE DATE!\n");
}

/*
 * Reset the TODR based on the time value; used when the TODR
 * has a preposterous value and also when the time is reset
 * by the stime system call.  Also called when the TODR goes past
 * TODRZERO + 100*(SECYEAR+2*SECDAY) (e.g. on Jan 2 just after midnight)
 * to wrap the TODR around.
 */
resettodr()
{

	(*cpuops->cpu_clock->clkwrite)();
}

/*
 * ``Standard'' VAX clock routines.
 */
#if VAX8600 || VAX8200 || VAX780 || VAX750 || VAX730
vaxstd_clkstartrt()
{

	mtpr(NICR, -1000000/hz);
	mtpr(ICCS, ICCS_RUN+ICCS_IE+ICCS_TRANS+ICCS_INT+ICCS_ERR);
}
#endif

#if VAX8600 || VAX780 || VAX750 || VAX730 || VAX650
vaxstd_clkread(base)
	time_t base;
{
	register u_int todr = mfpr(TODR);
	int year;

	/*
	 * TODRZERO is base used by VMS, which runs on local time.
	 */
	if (todr < TODRZERO) {
		printf("WARNING: todr too small");
		return (CLKREAD_BAD);
	}

	/*
	 * Sneak to within 6 months of the time in the filesystem,
	 * by starting with the time of the year suggested by the TODR,
	 * and advancing through succesive years.  Adding the number of
	 * seconds in the current year takes us to the end of the current year
	 * and then around into the next year to the same position.
	 */
	time.tv_sec = (todr - TODRZERO) / 100;
	year = YRREF;
	while (time.tv_sec < base - SECYR/2) {
		if (LEAPYEAR(year))
			time.tv_sec += SECDAY;
		year++;
		time.tv_sec += SECYR;
	}

	return (CLKREAD_OK);
}

vaxstd_clkwrite()
{
	int year = YRREF;
	u_int secyr;
	u_int yrtime = time.tv_sec;

	/*
	 * Whittle the time down to an offset in the current year,
	 * by subtracting off whole years as long as possible.
	 */
	for (;;) {
		secyr = SECYR;
		if (LEAPYEAR(year))
			secyr += SECDAY;
		if (yrtime < secyr)
			break;
		yrtime -= secyr;
		year++;
	}
	mtpr(TODR, TODRZERO + yrtime*100);
}
#endif

#if VAX8200 || VAX630
/*
 * This code is defunct after 2099.
 * Will Unix still be here then??
 */
short dayyr[12] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

chiptotime(c)
	register struct chiptime *c;
{
	register int days, yr;

	/* simple sanity checks */
	if (c->year < 70 || c->mon < 1 || c->mon > 12 ||
	    c->day < 1 || c->day > 31) {
		printf("WARNING: preposterous clock chip time");
		return (0);
	}
	days = 0;
	for (yr = 70; yr < c->year; yr++)
		days += LEAPYEAR(yr) ? 366 : 365;
	days += dayyr[c->mon - 1] + c->day - 1;
	if (LEAPYEAR(yr) && c->mon > 2)
		days++;
	/* now have days since Jan 1, 1970; the rest is easy... */
	return (days * SECDAY + c->hour * 3600 + c->min * 60 + c->sec);
}

timetochip(c)
	register struct chiptime *c;
{
	register int t, t2, t3;

	/* compute the year */
	t2 = time.tv_sec / SECDAY;
	t = 69;
	while (t2 >= 0) {	/* whittle off years */
		t3 = t2;
		t++;
		t2 -= LEAPYEAR(t) ? 366 : 365;
	}
	c->year = t;

	/* t3 = month + day; separate */
	t = LEAPYEAR(t);
	for (t2 = 1; t2 < 12; t2++)
		if (t3 < dayyr[t2] + (t && t2 > 1))
			break;

	/* t2 is month */
	c->mon = t2;
	c->day = t3 - dayyr[t2 - 1] + 1;
	if (t && t2 > 2)
		c->day--;

	/* the rest is easy */
	t = time.tv_sec % SECDAY;
	c->hour = t / 3600;
	t %= 3600;
	c->min = t / 60;
	c->sec = t % 60;
}
#endif
