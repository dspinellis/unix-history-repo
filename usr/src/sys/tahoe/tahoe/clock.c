/*	clock.c	1.2	86/01/05	*/

#include "../h/param.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../tahoe/clock.h"

/*
 * Initialze the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{

	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system");
		time.tv_sec = 6*SECYR + 186*SECDAY + SECDAY/2;
	} else
		time.tv_sec = base;
	printf(" CHECK AND RESET THE DATE!\n");
}
