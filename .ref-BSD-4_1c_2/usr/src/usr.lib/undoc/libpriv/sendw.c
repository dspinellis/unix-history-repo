/*	@(#)sendw.c	4.2	(Melbourne)	82/01/22	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/mu_msg.h>

sendw(smp, rmp, opt)
mmsgbuf *smp, *rmp;
{
	mmsgbuf mb; register r;

	mb = *smp;
	if ((r = mu_msg(MSG_SNDW, opt, &mb)) >= 0)
		*rmp = mb;
	return r;
}
