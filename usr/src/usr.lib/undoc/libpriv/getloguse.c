/*	@(#)getloguse.c	4.3	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <mushmuck.h>

long mushlreq();
short mushreq();

getloguse(uid, usage, maxuse, sess)
register uid;
long *usage, *maxuse;
short *sess;
{
	if (usage)
		*usage = mushlreq(uid, MM_A_USED);
	if (maxuse)
		*maxuse = mushlreq(uid, MM_A_MAXUSE);
	if (sess)
		*sess = mushreq(uid, MM_A_SESS);
}
