/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Added stuff to read the cmos clock on startup - Don Ahn
 *
 * %sccs.include.386.c%
 *
 *	@(#)clock.c	5.4 (Berkeley) %G%
 */

/*
 * Primitive clock interrupt routines.
 */
#include "param.h"
#include "time.h"
#include "kernel.h"
#include "machine/segments.h"
#include "machine/isa/icu.h"
#include "machine/isa/isa.h"

#define DAYST 119
#define DAYEN 303

startrtclock() {

	/* initialize 8253 clock */
	outb (IO_TIMER1+3, 0x36);
	outb (IO_TIMER1, 1193182/hz);
	outb (IO_TIMER1, (1193182/hz)/256);
}

/* convert 2 digit BCD number */
bcd(i)
int i;
{
	return ((i/16)*10 + (i%16));
}

/* convert years to seconds (from 1970) */
unsigned long
ytos(y)
int y;
{
	int i;
	unsigned long ret;

	ret = 0; y = y - 70;
	for(i=0;i<y;i++) {
		if (i % 4) ret += 31536000;
		else ret += 31622400;
	}
	return ret;
}

/* convert months to seconds */
unsigned long
mtos(m,leap)
int m,leap;
{
	int i;
	unsigned long ret;

	ret = 0;
	for(i=1;i<m;i++) {
		switch(i){
		case 1: case 3: case 5: case 7: case 8: case 10: case 12:
			ret += 2678400; break;
		case 4: case 6: case 9: case 11:
			ret += 2592000; break;
		case 2:
			if (leap) ret += 2505600;
			else ret += 2419200;
		}
	}
	return ret;
}


/*
 * Initialize the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{
	unsigned long sec;
	int leap,day_week,t,yd;

	sec = bcd(rtcin(9)); leap = !(sec % 4); sec += ytos(sec); /* year    */
	yd = mtos(bcd(rtcin(8)),leap); sec += yd;		/* month   */
	t = (bcd(rtcin(7))-1) * 86400; sec += t; yd += t;	/* date    */
	day_week = rtcin(6);					/* day     */
	sec += bcd(rtcin(4)) * 3600;				/* hour    */
	sec += bcd(rtcin(2)) * 60;				/* minutes */
	sec += bcd(rtcin(0));					/* seconds */

	/* XXX off by one? Need to calculate DST on SUNDAY */
	/* Perhaps we should have the RTC hold GMT time to save */
	/* us the bother of converting. */
	yd = yd / 86400;
	if ((yd >= DAYST) && ( yd <= DAYEN)) {
		sec -= 3600;
	}
	sec += tz.tz_minuteswest * 60;

	time.tv_sec = sec;
}

#ifdef garbage
/*
 * Initialze the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
test_inittodr(base)
	time_t base;
{

	outb(IO_RTC,9); /* year    */
	printf("%d ",bcd(inb(IO_RTC+1)));
	outb(IO_RTC,8); /* month   */
	printf("%d ",bcd(inb(IO_RTC+1)));
	outb(IO_RTC,7); /* day     */
	printf("%d ",bcd(inb(IO_RTC+1)));
	outb(IO_RTC,4); /* hour    */
	printf("%d ",bcd(inb(IO_RTC+1)));
	outb(IO_RTC,2); /* minutes */
	printf("%d ",bcd(inb(IO_RTC+1)));
	outb(IO_RTC,0); /* seconds */
	printf("%d\n",bcd(inb(IO_RTC+1)));

	time.tv_sec = base;
}
#endif

/*
 * retreve a value from realtime clock
 */
u_char rtcin(n) {
	u_char val;

	outb(IO_RTC,n);
	do val = inb(IO_RTC+1) ; while (val != inb(IO_RTC+1));
	return (val);
}

/*
 * Restart the clock.
 */
resettodr()
{
}

#define V(s)	V/**/s
extern V(clk)();
enablertclock() {
	INTREN(IRQ0);
	setidt(ICU_OFFSET+0, &V(clk), SDT_SYS386IGT, SEL_KPL);
	splnone();
}
