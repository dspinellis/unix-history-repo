/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1982, 1990, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: clockreg.h 1.14 91/01/18$
 * from: hp300/hp300/clockreg.h	7.4 (Berkeley) 12/27/92
 *
 *	@(#)clockreg.h	8.2 (Berkeley) 8/15/93
 */

/*
 *  LUNA system clock defines
 */
#define	CLOCK_REG	0x63000000	/* system clock address */

#define	CLK_INT		0x7		/* system clock intr flag */
#define	CLK_CLR		0x1		/* system clock intr clear */

/*
 * LUNA battery-backed clock
 */

/* only use software */
struct bbc_tm {
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
};

#define FEBRUARY	2
#define	STARTOFTIME	1970
#define SECDAY		86400L
#define SECYR		(SECDAY * 365)

#define	leapyear(year)		((year) % 4 == 0)
#define	range_test(n, l, h)	if ((n) < (l) || (n) > (h)) return(0)
#define	days_in_year(a) 	(leapyear(a) ? 366 : 365)
#define	days_in_month(a) 	(month_days[(a) - 1])

/*
 * TIME KEEPER RAM -- (MK48T02/12(B)-12/15/20/25)
 */

#include <luna68k/dev/nvram.h>

struct bbc {
    struct nvram 	nvram;	       	/* non-volatile RAM area */
    unsigned char    	cal_ctl;       	/* calender control resistor */
    unsigned char	cal_sec;       	/* secons resistor */
    unsigned char	cal_min;       	/* minutes resistor */
    unsigned char	cal_hour;      	/* hours resitor */
    unsigned char	cal_dow;       	/* day of the weeks */
    unsigned char	cal_day;       	/* days resistor */
    unsigned char	cal_mon;       	/* months resistor */
    unsigned char	cal_year;      	/* years resistor */
};

#define	BBC_ADDR	0x45000000	/* battery backuped clock address */

#define	BBC_FRQ		0x40		/* Frequency test (in day) */
#define	BBC_KICK	0x80		/* Kick start (in hour) */
#define	BBC_STOP	0x80		/* Stop bit (in seconds) */
#define	BBC_WRT		0x80		/* Write bit (in control) */
#define	BBC_RD		0x40		/* Read bit (in control) */
#define	BBC_SGN		0x20		/* Sign bit (in control) */
#define	BBC_DELAY	180		/* delay time */

#define	binary_to_bcd(i)	(((i) / 10) << 4 | ((i) % 10))
#define	bcd_to_binary(i)	(((i) >> 4) *10 + ((i) & 0x0F))

#ifdef LUNA2
struct bbc2 {
    unsigned char	cal_sec;	/* secons resistor */
    unsigned char	cal_sec_alarm;	/* secons alarm resistor */
    unsigned char	cal_min;	/* minutes resistor */
    unsigned char	cal_min_alarm;	/* minutes alarm resistor */
    unsigned char	cal_hour;	/* hours resitor */
    unsigned char	cal_hour_alarm;	/* hours alarm resitor */
    unsigned char	cal_dow;	/* day of the weeks */
    unsigned char	cal_day;	/* days resistor */
    unsigned char	cal_mon;	/* months resistor */
    unsigned char	cal_year;	/* years resistor */
    unsigned char    	cal_ctl_a;	/* calender control resistor */
    unsigned char    	cal_ctl_b;	/* calender control resistor */
    unsigned char    	cal_ctl_c;	/* calender control resistor */
    unsigned char    	cal_ctl_d;	/* calender control resistor */
    unsigned char    	nvram[50];	/* non-volatile RAM area */
};

#define	BBC2_A_UIP	0x80
#define	BBC2_B_SET	0x80
#endif
