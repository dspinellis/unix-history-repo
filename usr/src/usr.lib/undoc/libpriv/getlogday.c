/*	@(#)getlogday.c	4.3	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <mushmuck.h>

long mushlreq();
short mushreq();

getlogday(uid, dmax, dinc, drem)
register uid;
short *dmax, *dinc, *drem;
{
	if (dmax)
		*dmax = mushreq(uid, MM_A_DMAX);
	if (dinc)
		*dinc = mushreq(uid, MM_A_DINC);
	if (drem)
		*drem = mushreq(uid, MM_A_DREM);
}
