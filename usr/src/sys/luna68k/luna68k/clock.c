/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1982, 1990, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clock.c 1.18 91/01/21$
 * from: hp300/hp300/clock.c	7.17 (Berkeley) 12/27/92
 *
 *	@(#)clock.c	7.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/kernel.h>

#include <luna68k/luna68k/clockreg.h>

extern int clock_on;

static int month_days[12] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};
struct bbc_tm *gmt_to_bbc();

volatile struct bbc *bbc = (struct bbc *)BBC_ADDR;

int battery_clock;
int battery_chkfg;

/*
 * Machine-dependent clock routines.
 *
 * Startrtclock just checks battry backuped clock
 * (when it does not work, starts it).
 *
 * Enablertclock sets flag for clock interrupt.
 *
 * Inittodr initializes the time of day hardware which provides
 * date functions.
 *
 * Resettodr restores the time of day hardware after a time change.
 *
 */

/*
 * Start the real-time clock.
 */
cpu_initclocks()
{
	static char *rtcstrings = "RTC";     /* For compat */

	/* set flag for clockintr. */
	clock_on = 1;

	batterychk();
	if (!battery_clock)
	  return;

	if (!strncmp(bbc->nvram.nv_calclock, rtcstrings, sizeof(rtcstrings))) /* Okey */
	  return;
	
	printf("Initialize Battery Backup Clock.\n");
	bbc->cal_ctl |= BBC_WRT;
	bbc->cal_sec &= ~BBC_STOP;
	bbc->cal_hour |= BBC_KICK;
	bbc->cal_dow &= ~BBC_FRQ;
	bbc->cal_ctl &= ~BBC_WRT;
	DELAY(BBC_DELAY);
	bbc->cal_ctl |= BBC_WRT;
	bbc->cal_hour &= ~BBC_KICK;
	bbc->cal_ctl &= ~BBC_WRT;
	strcpy(bbc->nvram,rtcstrings);
}

void
setstatclockrate(newhz)
        int newhz;
{
}

microtime(tvp)
        register struct timeval *tvp;
{
        int s = splhigh();

        *tvp = time;
        tvp->tv_usec += tick;
        while (tvp->tv_usec > 1000000) {
                tvp->tv_sec++;
                tvp->tv_usec -= 1000000;
        }
        splx(s);
}

/*
 * Initialize the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{
	u_long timbuf = base;	/* assume no battery clock exists */

	/*
	 * bbc_to_gmt converts and stores the gmt in timbuf.
	 * If an error is detected in bbc_to_gmt, or if the filesystem
	 * time is more recent than the gmt time in the clock,
	 * then use the filesystem time and warn the user.
 	 */
	if (!bbc_to_gmt(&timbuf) || timbuf < base) {
		printf("WARNING: bad date in battery clock\n");
		timbuf = base;
	}
	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system");
		timbuf = 6*SECYR + 186*SECDAY + SECDAY/2;
		printf(" -- CHECK AND RESET THE DATE!\n");
	}
	
	/* Battery clock does not store usec's, so forget about it. */
	time.tv_sec = timbuf;
}

resettodr()
{
	register int i,s;
	register struct bbc_tm *tmptr;

	tmptr = gmt_to_bbc(time.tv_sec);

	s = splimp();

	/* set bb-clock */
	bbc->cal_ctl |= BBC_WRT;
	bbc->cal_sec = binary_to_bcd(tmptr->tm_sec);
	bbc->cal_min = binary_to_bcd(tmptr->tm_min);
	bbc->cal_hour = binary_to_bcd(tmptr->tm_hour);
	bbc->cal_day = binary_to_bcd(tmptr->tm_mday);
	bbc->cal_mon = binary_to_bcd(tmptr->tm_mon);
	bbc->cal_year = binary_to_bcd(tmptr->tm_year);
	bbc->cal_ctl &= ~BBC_WRT;

	splx(s);
}

struct bbc_tm *
gmt_to_bbc(tim)
	long tim;
{
	register int i;
	register long hms, day;
	static struct bbc_tm rt;

	day = tim / SECDAY;
	hms = tim % SECDAY;

	/* Hours, minutes, seconds are easy */
	rt.tm_hour = hms / 3600;
	rt.tm_min  = (hms % 3600) / 60;
	rt.tm_sec  = (hms % 3600) % 60;

	/* Number of years in days */
	for (i = STARTOFTIME - 1900; day >= days_in_year(i); i++)
	  	day -= days_in_year(i);
	rt.tm_year = i;
	
	/* Number of months in days left */
	if (leapyear(rt.tm_year))
		days_in_month(FEBRUARY) = 29;
	for (i = 1; day >= days_in_month(i); i++)
		day -= days_in_month(i);
	days_in_month(FEBRUARY) = 28;
	rt.tm_mon = i;

	/* Days are what is left over (+1) from all that. */
	rt.tm_mday = day + 1;  
	
	return(&rt);
}

bbc_to_gmt(timbuf)
	u_long *timbuf;
{
	register int i,s;
	register u_long tmp;
	int year, month, day, hour, min, sec;

        if (!battery_clock)
	   return(0);

	s = splimp();

	/* read bb-clock */
	bbc->cal_ctl |= BBC_RD;
	sec = bcd_to_binary(bbc->cal_sec);
	min = bcd_to_binary(bbc->cal_min);
	hour = bcd_to_binary(bbc->cal_hour);
	day = bcd_to_binary(bbc->cal_day);
	month = bcd_to_binary(bbc->cal_mon);
	year = bcd_to_binary(bbc->cal_year) + 1900;
	bbc->cal_ctl &= ~BBC_RD;
	
	splx(s);

	range_test(hour, 0, 23);
	range_test(day, 1, 31);
	range_test(month, 1, 12);
	range_test(year, STARTOFTIME, 2000);
	
	tmp = 0;
	
	for (i = STARTOFTIME; i < year; i++)
	    tmp += days_in_year(i);
	if (leapyear(year) && month > FEBRUARY)
	    tmp++;

	for (i = 1; i < month; i++)
	    tmp += days_in_month(i);
	
	tmp += (day - 1);
	tmp = ((tmp * 24 + hour) * 60 + min) * 60 + sec;

	*timbuf = tmp;
	return(1);
}

batterychk()
{
	static char btchkdata[] = "chk";

	/* if already checked, return */
	if (battery_chkfg)
		return;

	battery_chkfg = 1;
	if (badaddr((caddr_t)bbc, 2))
		return;

	strcpy(bbc->nvram.nv_testwrite, btchkdata);
	if (strncmp(bbc->nvram.nv_testwrite, btchkdata, sizeof(btchkdata))) {
		printf("WARNING: calendar clock battery down\n");
		return;
	}
	battery_clock = 1;
	return;
}
