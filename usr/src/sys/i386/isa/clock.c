/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz and Don Ahn.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	from: @(#)clock.c	7.2 (Berkeley) 5/12/91
 *	from NetBSD: Id: clock.c,v 1.6 1993/05/22 08:01:07 cgd Exp 
 *
 *	@(#)clock.c	8.1 (Berkeley) 6/11/93
 *
 */

/*
 * Primitive clock interrupt routines.
 */
#include <sys/param.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <machine/segments.h>
#include <i386/isa/icu.h>
#include <i386/isa/isa.h>
#include <i386/isa/rtc.h>

/* these should go elsewere (timerreg.h) but to avoid admin overhead... */
/*
 * Macros for specifying values to be written into a mode register.
 */
#define TIMER_CNTR0     (IO_TIMER1 + 0) /* timer 0 counter port */
#define TIMER_CNTR1     (IO_TIMER1 + 1) /* timer 1 counter port */
#define TIMER_CNTR2     (IO_TIMER1 + 2) /* timer 2 counter port */
#define TIMER_MODE      (IO_TIMER1 + 3) /* timer mode port */
#define         TIMER_SEL0      0x00    /* select counter 0 */
#define         TIMER_SEL1      0x40    /* select counter 1 */
#define         TIMER_SEL2      0x80    /* select counter 2 */
#define         TIMER_INTTC     0x00    /* mode 0, intr on terminal cnt */
#define         TIMER_ONESHOT   0x02    /* mode 1, one shot */
#define         TIMER_RATEGEN   0x04    /* mode 2, rate generator */
#define         TIMER_SQWAVE    0x06    /* mode 3, square wave */
#define         TIMER_SWSTROBE  0x08    /* mode 4, s/w triggered strobe */
#define         TIMER_HWSTROBE  0x0a    /* mode 5, h/w triggered strobe */
#define         TIMER_LATCH     0x00    /* latch counter for reading */
#define         TIMER_LSB       0x10    /* r/w counter LSB */
#define         TIMER_MSB       0x20    /* r/w counter MSB */
#define         TIMER_16BIT     0x30    /* r/w counter 16 bits, LSB first */
#define         TIMER_BCD       0x01    /* count in BCD */

#define DAYST 119
#define DAYEN 303

#ifndef	XTALSPEED
#define XTALSPEED 1193182
#endif

startrtclock() {
	int s;

	findcpuspeed();		/* use the clock (while it's free)
					to find the cpu speed */
	/* initialize 8253 clock */
	outb(TIMER_MODE, TIMER_SEL0|TIMER_RATEGEN|TIMER_16BIT);

	/* Correct rounding will buy us a better precision in timekeeping */
	outb (IO_TIMER1, (XTALSPEED+hz/2)/hz);
	outb (IO_TIMER1, ((XTALSPEED+hz/2)/hz)/256);

	/* initialize brain-dead battery powered clock */
	outb (IO_RTC, RTC_STATUSA);
	outb (IO_RTC+1, 0x26);
	outb (IO_RTC, RTC_STATUSB);
	outb (IO_RTC+1, 2);

	outb (IO_RTC, RTC_DIAG);
	if (s = inb (IO_RTC+1))
		printf("RTC BIOS diagnostic error %b\n", s, RTCDG_BITS);
	outb (IO_RTC, RTC_DIAG);
	outb (IO_RTC+1, 0);
}

unsigned int delaycount;	/* calibrated loop variable (1 millisecond) */

#define FIRST_GUESS	0x2000
findcpuspeed()
{
	unsigned char low;
	unsigned int remainder;

	/* Put counter in count down mode */
	outb(IO_TIMER1+3, 0x34);
	outb(IO_TIMER1, 0xff);
	outb(IO_TIMER1, 0xff);
	delaycount = FIRST_GUESS;
	spinwait(1);
	/* Read the value left in the counter */
	low 	= inb(IO_TIMER1);	/* least siginifcant */
	remainder = inb(IO_TIMER1);	/* most significant */
	remainder = (remainder<<8) + low ;
	/* Formula for delaycount is :
	 *  (loopcount * timer clock speed)/ (counter ticks * 1000)
	 */
	delaycount = (FIRST_GUESS * (XTALSPEED/1000)) / (0xffff-remainder);
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

	ret = 0;
	for(i = 1970; i < y; i++) {
		if (i % 4) ret += 365*24*60*60;
		else ret += 366*24*60*60;
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
			ret += 31*24*60*60; break;
		case 4: case 6: case 9: case 11:
			ret += 30*24*60*60; break;
		case 2:
			if (leap) ret += 29*24*60*60;
			else ret += 28*24*60*60;
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
	int sa,s;

	/* do we have a realtime clock present? (otherwise we loop below) */
	sa = rtcin(RTC_STATUSA);
	if (sa == 0xff || sa == 0) return;

	/* ready for a read? */
	while ((sa&RTCSA_TUP) == RTCSA_TUP)
		sa = rtcin(RTC_STATUSA);

	sec = bcd(rtcin(RTC_YEAR)) + 1900;
	if (sec < 1970)
		sec += 100;
	leap = !(sec % 4); sec = ytos(sec); /* year    */
	yd = mtos(bcd(rtcin(RTC_MONTH)),leap); sec += yd;	/* month   */
	t = (bcd(rtcin(RTC_DAY))-1) * 24*60*60; sec += t; yd += t; /* date    */
	day_week = rtcin(RTC_WDAY);				/* day     */
	sec += bcd(rtcin(RTC_HRS)) * 60*60;			/* hour    */
	sec += bcd(rtcin(RTC_MIN)) * 60;			/* minutes */
	sec += bcd(rtcin(RTC_SEC));				/* seconds */

	/* XXX off by one? Need to calculate DST on SUNDAY */
	/* Perhaps we should have the RTC hold GMT time to save */
	/* us the bother of converting. */
	yd = yd / (24*60*60);
	if ((yd >= DAYST) && ( yd <= DAYEN)) {
		sec -= 60*60;
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
 * Restart the clock.
 */
resettodr()
{
}

/*
 * Wire clock interrupt in.
 */
#define V(s)	__CONCAT(V, s)
extern V(clk)();
enablertclock() {
	INTREN(IRQ0);
	setidt(ICU_OFFSET+0, &V(clk), SDT_SYS386IGT, SEL_KPL);
	splnone();
}




spinwait(millisecs)
int millisecs;		/* number of milliseconds to delay */
{
	int i, j;

	for (i=0;i<millisecs;i++)
		for (j=0;j<delaycount;j++)
			;
}

