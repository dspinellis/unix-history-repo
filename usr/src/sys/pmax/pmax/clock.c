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
 * from: Utah $Hdr: clock.c 1.18 91/01/21$
 *
 *	@(#)clock.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "kernel.h"

#include "../include/machConst.h"
#include "clockreg.h"

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
	register volatile struct chiptime *c;
	extern int tickadj;

	tick = 15625;		/* number of micro-seconds between interrupts */
	hz = 1000000 / 15625;	/* about 64 */
	tickadj = 240000 / (60000000 / 15625);
	c = (volatile struct chiptime *)MACH_CLOCK_ADDR;
	c->rega = REGA_TIME_BASE | SELECTED_RATE;
	c->regb = REGB_PER_INT_ENA | REGB_DATA_MODE | REGB_HOURS_FORMAT;
}

/*
 * This code is defunct after 2099.
 * Will Unix still be here then??
 */
static short dayyr[12] = {
	0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
};

/*
 * Initialze the time of day register, based on the time base which is, e.g.
 * from a filesystem.  Base provides the time to within six months,
 * and the time of year clock (if any) provides the rest.
 */
inittodr(base)
	time_t base;
{
	register volatile struct chiptime *c;
	register int days, yr;
	int sec, min, hour, day, mon, year;
	long deltat, badbase = 0;
	int s;

	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system\n");
		/* read the system clock anyway */
		base = 6*SECYR + 186*SECDAY + SECDAY/2;
		badbase = 1;
	}

	c = (volatile struct chiptime *)MACH_CLOCK_ADDR;
	/* don't read clock registers while they are being updated */
	s = splclock();
	while ((c->rega & REGA_UIP) == 1)
		;
	sec = c->sec;
	min = c->min;
	hour = c->hour;
	day = c->day;
	mon = c->mon;
	year = c->year + 19;
	splx(s);

	/* simple sanity checks */
	if (year < 70 || mon < 1 || mon > 12 || day < 1 || day > 31 ||
	    hour > 23 || min > 59 || sec > 59) {
		printf("WARNING: preposterous clock chip time");
		/*
		 * Believe the time in the file system for lack of
		 * anything better, resetting the TODR.
		 */
		time.tv_sec = base;
		if (!badbase)
			resettodr();
		return (0);
	}
	days = 0;
	for (yr = 70; yr < year; yr++)
		days += LEAPYEAR(yr) ? 366 : 365;
	days += dayyr[mon - 1] + day - 1;
	if (LEAPYEAR(yr) && mon > 2)
		days++;
	/* now have days since Jan 1, 1970; the rest is easy... */
	time.tv_sec = days * SECDAY + hour * 3600 + min * 60 + sec;

	if (!badbase) {
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
	register volatile struct chiptime *c;
	register int t, t2, t3;
	int s;

	/* compute the year */
	t2 = time.tv_sec / SECDAY;
	t = 69;
	while (t2 >= 0) {	/* whittle off years */
		t3 = t2;
		t++;
		t2 -= LEAPYEAR(t) ? 366 : 365;
	}

	c = (volatile struct chiptime *)MACH_CLOCK_ADDR;
	s = splclock();
	c->regb |= REGB_SET_TIME;
	c->year = t - 19;

	/* t3 = month + day; separate */
	t = LEAPYEAR(t);
	for (t2 = 1; t2 < 12; t2++)
		if (t3 < dayyr[t2] + (t && t2 > 1))
			break;

	/* t2 is month */
	c->mon = t2;
	t3 = t3 - dayyr[t2 - 1] + 1;
	if (t && t2 > 2)
		t3--;
	c->day = t3;

	/* the rest is easy */
	t = time.tv_sec % SECDAY;
	c->hour = t / 3600;
	t %= 3600;
	c->min = t / 60;
	c->sec = t % 60;
	c->regb &= ~REGB_SET_TIME;
	MachEmptyWriteBuffer();
	splx(s);
}
