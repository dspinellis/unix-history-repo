/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)clock.c	7.2 (Berkeley) 5/12/91
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00044
 * --------------------         -----   ----------------------
 *
 * 14 Aug 92	Arne Henrik Juul	Added code for the kernel to
 *					allow for DST in the BIOS.
 */

/*
 * Primitive clock interrupt routines.
 */
#include "param.h"
#include "time.h"
#include "kernel.h"
#include "machine/segments.h"
#include "i386/isa/icu.h"
#include "i386/isa/isa.h"
#include "i386/isa/rtc.h"

#define DAYST 119
#define DAYEN 303

startrtclock() {
	int s;

	/* initialize 8253 clock */
	outb (IO_TIMER1+3, 0x36);
	outb (IO_TIMER1, 1193182/hz);
	outb (IO_TIMER1, (1193182/hz)/256);

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

	sec = bcd(rtcin(RTC_YEAR));
	leap = !(sec % 4); sec += ytos(sec); /* year    */
	yd = mtos(bcd(rtcin(RTC_MONTH)),leap); sec += yd;	/* month   */
	t = (bcd(rtcin(RTC_DAY))-1) * 24*60*60; sec += t; yd += t; /* date    */
	day_week = rtcin(RTC_WDAY);				/* day     */
	sec += bcd(rtcin(RTC_HRS)) * 60*60;			/* hour    */
	sec += bcd(rtcin(RTC_MIN)) * 60;			/* minutes */
	sec += bcd(rtcin(RTC_SEC));				/* seconds */
	sec -= 24*60*60; /* XXX why ??? */

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
