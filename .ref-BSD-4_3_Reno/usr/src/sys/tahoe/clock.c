/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)clock.c	7.3 (Berkeley) 6/28/90
 */

#include "param.h"
#include "time.h"
#include "kernel.h"

#include "pte.h"
#include "cpu.h"
#include "mtpr.h"
#include "clock.h"
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
