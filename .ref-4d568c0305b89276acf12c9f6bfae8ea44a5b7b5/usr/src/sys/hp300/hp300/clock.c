/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clock.c 1.18 91/01/21$
 *
 *	@(#)clock.c	8.2 (Berkeley) %G%
 */

/*
 * HPs use the MC6840 PTM with the following arrangement:
 *	Timers 1 and 3 are externally driver from a 25Mhz source.
 *	Output from timer 3 is tied to the input of timer 2.
 * The latter makes it possible to use timers 3 and 2 together to get
 * a 32-bit countdown timer.
 */

#include <sys/param.h>
#include <sys/kernel.h>
#include <hp/dev/hilreg.h>
#include <hp300/hp300/clockreg.h>

#include <machine/psl.h>
#include <machine/cpu.h>

#ifdef GPROF
#include <sys/gmon.h>
#endif

int    clkstd[1];

static int clkint;		/* clock interval, as loaded */
/*
 * Statistics clock interval and variance, in usec.  Variance must be a
 * power of two.  Since this gives us an even number, not an odd number,
 * we discard one case and compensate.  That is, a variance of 1024 would
 * give us offsets in [0..1023].  Instead, we take offsets in [1..1023].
 * This is symmetric about the point 512, or statvar/2, and thus averages
 * to that value (assuming uniform random numbers).
 */
static int statvar = 1024 / 4;	/* {stat,prof}clock variance */
static int statmin;		/* statclock interval - variance/2 */
static int profmin;		/* profclock interval - variance/2 */
static int timer3min;		/* current, from above choices */
static int statprev;		/* previous value in stat timer */

static int month_days[12] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};
struct bbc_tm *gmt_to_bbc();
u_char bbc_registers[13];
u_char write_bbc_reg(), read_bbc_reg();
struct hil_dev *bbcaddr = NULL;

/*
 * Machine-dependent clock routines.
 *
 * A note on the real-time clock:
 * We actually load the clock with interval-1 instead of interval.
 * This is because the counter decrements to zero after N+1 enabled clock
 * periods where N is the value loaded into the counter.
 *
 * The frequencies of the HP300 clocks must be a multiple of four
 * microseconds (since the clock counts in 4 us units).
 */
#define	COUNTS_PER_SEC	(1000000 / CLK_RESOLUTION)

/*
 * Set up the real-time and statistics clocks.  Leave stathz 0 only if
 * no alternative timer is available.
 *
 */
cpu_initclocks()
{
	register volatile struct clkreg *clk;
	register int intvl, statint, profint, minint;

	clkstd[0] = IIOV(0x5F8000);		/* XXX grot */
	clk = (volatile struct clkreg *)clkstd[0];

	if (COUNTS_PER_SEC % hz) {
		printf("cannot get %d Hz clock; using 100 Hz\n", hz);
		hz = 100;
	}
	/*
	 * Clock has several counters, so we can always use separate
	 * statclock.
	 */
	if (stathz == 0)		/* XXX should be set in param.c */
		stathz = hz;
	else if (COUNTS_PER_SEC % stathz) {
		printf("cannot get %d Hz statclock; using 100 Hz\n", stathz);
		stathz = 100;
	}
	if (profhz == 0)		/* XXX should be set in param.c */
		profhz = stathz * 5;
	else if (profhz < stathz || COUNTS_PER_SEC % profhz) {
		printf("cannot get %d Hz profclock; using %d Hz\n",
		    profhz, stathz);
		profhz = stathz;
	}

	intvl = COUNTS_PER_SEC / hz;
	statint = COUNTS_PER_SEC / stathz;
	profint = COUNTS_PER_SEC / profhz;
	minint = statint / 2 + 100;
	while (statvar > minint)
		statvar >>= 1;

	tick = intvl * CLK_RESOLUTION;

	/* adjust interval counts, per note above */
	intvl--;
	statint--;
	profint--;

	/* calculate base reload values */
	clkint = intvl;
	statmin = statint - (statvar >> 1);
	profmin = profint - (statvar >> 1);
	timer3min = statmin;
	statprev = statint;

	/* finally, load hardware */
	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_RESET;
	asm volatile(" movpw %0,%1@(5)" : : "d" (intvl), "a" (clk));
	asm volatile(" movpw %0,%1@(9)" : : "d" (0), "a" (clk));
	asm volatile(" movpw %0,%1@(13)" : : "d" (statint), "a" (clk));
	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_IENAB;
	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = CLK_IENAB;
}

/*
 * We assume newhz is either stathz or profhz, and that neither will
 * change after being set up above.  Could recalculate intervals here
 * but that would be a drag.
 */
void
setstatclockrate(newhz)
	int newhz;
{

	if (newhz == stathz)
		timer3min = statmin;
	else
		timer3min = profmin;
}

/*
 * Statistics/profiling clock interrupt.  Compute a new interval.
 * Interrupt has already been cleared.
 *
 * DO THIS INLINE IN locore.s?
 */
void
statintr(fp)
	struct clockframe *fp;
{
	register volatile struct clkreg *clk;
	register int newint, r, var;

	clk = (volatile struct clkreg *)clkstd[0];
	var = statvar;
	do {
		r = random() & (var - 1);
	} while (r == 0);
	newint = timer3min + r;

	/*
	 * The timer was automatically reloaded with the previous latch
	 * value at the time of the interrupt.  Compensate now for the
	 * amount of time that has run off since then (minimum of 2-12
	 * timer ticks depending on CPU type) plus one tick roundoff.
	 * This should keep us closer to the mean.
	 */
	asm volatile(" clrl %0; movpw %1@(13),%0" : "=d" (r) : "a" (clk));
	newint -= (statprev - r + 1);

	asm volatile(" movpw %0,%1@(13)" : : "d" (newint), "a" (clk));
	statprev = newint;
	statclock(fp);
}

/*
 * Return the best possible estimate of the current time.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	register volatile struct clkreg *clk;
	register int s, u, t, u2, s2;

	/*
	 * Read registers from slowest-changing to fastest-changing,
	 * then re-read out to slowest.  If the values read before the
	 * innermost match those read after, the innermost value is
	 * consistent with the outer values.  If not, it may not be and
	 * we must retry.  Typically this loop runs only once; occasionally
	 * it runs twice, and only rarely does it run longer.
	 *
	 * (Using this loop avoids the need to block interrupts.)
	 */
	clk = (volatile struct clkreg *)clkstd[0];
	do {
		s = time.tv_sec;
		u = time.tv_usec;
		asm volatile (" clrl %0; movpw %1@(5),%0"
			      : "=d" (t) : "a" (clk));
		u2 = time.tv_usec;
		s2 = time.tv_sec;
	} while (u != u2 || s != s2);

	u += (clkint - t) * CLK_RESOLUTION;
	if (u >= 1000000) {		/* normalize */
		s++;
		u -= 1000000;
	}
	tvp->tv_sec = s;
	tvp->tv_usec = u;
}

/*
 * Initialize the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{
	u_long timbuf = base;	/* assume no battery clock exists */
	static int bbcinited = 0;

	/* XXX */
	if (!bbcinited) {
		if (badbaddr(&BBCADDR->hil_stat))
			printf("WARNING: no battery clock\n");
		else
			bbcaddr = BBCADDR;
		bbcinited = 1;
	}

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

/*
 * Restore the time of day hardware after a time change.
 */
resettodr()
{
	register int i;
	register struct bbc_tm *tmptr;

	tmptr = gmt_to_bbc(time.tv_sec);

	decimal_to_bbc(0, 1,  tmptr->tm_sec);
	decimal_to_bbc(2, 3,  tmptr->tm_min);
	decimal_to_bbc(4, 5,  tmptr->tm_hour);
	decimal_to_bbc(7, 8,  tmptr->tm_mday);
	decimal_to_bbc(9, 10, tmptr->tm_mon);
	decimal_to_bbc(11, 12, tmptr->tm_year);

	/* Some bogusness to deal with seemingly broken hardware. Nonsense */
	bbc_registers[5] = ((tmptr->tm_hour / 10) & 0x03) + 8;

	write_bbc_reg(15, 13);	/* reset prescalar */

	for (i = 0; i <= NUM_BBC_REGS; i++)
	  	if (bbc_registers[i] != write_bbc_reg(i, bbc_registers[i])) {
			printf("Cannot set battery backed clock\n");
			break;
		}
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
	register int i;
	register u_long tmp;
	int year, month, day, hour, min, sec;

	read_bbc();

	sec = bbc_to_decimal(1, 0);
	min = bbc_to_decimal(3, 2);

	/*
	 * Hours are different for some reason. Makes no sense really.
	 */
	hour  = ((bbc_registers[5] & 0x03) * 10) + bbc_registers[4];
	day   = bbc_to_decimal(8, 7);
	month = bbc_to_decimal(10, 9);
	year  = bbc_to_decimal(12, 11) + 1900;

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

read_bbc()
{
  	register int i, read_okay;

	read_okay = 0;
	while (!read_okay) {
		read_okay = 1;
		for (i = 0; i <= NUM_BBC_REGS; i++)
			bbc_registers[i] = read_bbc_reg(i);
		for (i = 0; i <= NUM_BBC_REGS; i++)
			if (bbc_registers[i] != read_bbc_reg(i))
				read_okay = 0;
	}
}

u_char
read_bbc_reg(reg)
	int reg;
{
	u_char data = reg;

	if (bbcaddr) {
		send_hil_cmd(bbcaddr, BBC_SET_REG, &data, 1, NULL);
		send_hil_cmd(bbcaddr, BBC_READ_REG, NULL, 0, &data);
	}
	return(data);
}

u_char
write_bbc_reg(reg, data)
	int reg;
	u_int data;
{
	u_char tmp;

	tmp = (u_char) ((data << HIL_SSHIFT) | reg);

	if (bbcaddr) {
		send_hil_cmd(bbcaddr, BBC_SET_REG, &tmp, 1, NULL);
		send_hil_cmd(bbcaddr, BBC_WRITE_REG, NULL, 0, NULL);
		send_hil_cmd(bbcaddr, BBC_READ_REG, NULL, 0, &tmp);
	}
	return(tmp);
}	
