/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)clock.c	7.4 (Berkeley) 12/16/90
 */

#include "sys/param.h"
#include "sys/time.h"
#include "sys/kernel.h"

#include "../include/pte.h"
#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "../include/clock.h"
#include "cp.h"

/*
 * Machine-dependent clock routines.
 *
 * Startrtclock restarts the real-time clock, which provides
 * hardclock interrupts to kern_clock.c.
 *
 * Inittodr initializes any time of day hardware which provides
 * date functions.  Its primary function is to use some file
 * system information in case the hardare clock lost state.
 */

/*
 * Start the real-time clock by initializing the
 * interval timer on the console processor card
 * according to hz.
 */
startrtclock()
{
	register int t;
	static struct cphdr cpclock;	/* must be in data space */
	
	cpclock.cp_unit = CPCLOCK;
	cpclock.cp_comm = CPWRITE;
	if (hz == 0) {
		extern int tickadj;		/* XXX */
		hz = 60;
		tick = 1000000 / hz;
		tickadj = 240000 / (60 * hz);
		printf("clock set to %dhz\n", hz);
	}
	cpclock.cp_count = hztocount(hz);
	if (cnlast) {
		/* try to insure last cmd was taken by cp */
		for (t = 30000; (cnlast->cp_unit&CPTAKE) == 0 && t > 0; t--)
			uncache(&cnlast->cp_unit);
		cnlast = 0;
	}
	mtpr(CPMDCB, kvtophys(&cpclock));
	for (t = 30000; (cpclock.cp_unit&CPDONE) == 0 && t > 0; t--)
		uncache(&cpclock.cp_unit);
}

/*
 * Initialze the time of day register, based on
 * the time base which is, e.g. from a filesystem.
 */
inittodr(base)
	time_t base;
{

	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system ");
		time.tv_sec = 6*SECYR + 186*SECDAY + SECDAY/2;
	} else
		time.tv_sec = base;
	printf("CHECK AND RESET THE DATE!\n");
}
