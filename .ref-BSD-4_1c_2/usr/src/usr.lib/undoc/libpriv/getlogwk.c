/*	@(#)getlogwk.c	4.3	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <mushmuck.h>

long mushlreq();
short mushreq();

getlogwk(uid, wmax, winc, wrem)
register uid;
short *wmax, *winc, *wrem;
{
	if (wmax)
		*wmax = mushreq(uid, MM_A_WMAX);
	if (winc)
		*winc = mushreq(uid, MM_A_WINC);
	if (wrem)
		*wrem = mushreq(uid, MM_A_WREM);
}
