/*	clock.c	1.3	86/12/06	*/

#include "param.h"
#include "time.h"
#include "kernel.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/clock.h"
#include "../tahoe/cp.h"

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
	struct cphdr cpclock;
	
	cpclock.cp_unit = CPCLOCK;
	cpclock.cp_comm = CPWRITE;
	if (hz == 0) {
		hz = 60;
		printf("clock set to %dhz\n", hz);
	}
	cpclock.cp_count = hztocount(hz);
	/* try to insure last cmd completed */
	if (cnlast) {
		for (t = 30000; (cnlast->cp_unit&CPTAKE) == 0 && t > 0; t--)
			uncache(&cnlast->cp_unit);
		cnlast = 0;
	}
	mtpr(CPMDCB, &cpclock);
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
