/*	@(#)getlogsess.c	4.3	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <mushmuck.h>

long mushlreq();
short mushreq();

getlogsess(uid, smax, lgap, left)
register uid;
short *smax, *lgap, *left;
{
	if (smax)
		*smax = mushreq(uid, MM_A_SMAX);
	if (lgap)
		*lgap = mushreq(uid, MM_A_LGAP);
	if (left)
		*left = mushreq(uid, MM_A_LEFT);
}
