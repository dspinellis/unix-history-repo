/*	@(#)getshare.c	4.2	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/mu_msg.h>
#include <sys/quota.h>
#include <mushmuck.h>

float
getshare(uid)
{
	mmsgbuf	mb;

	mb.msg_pid = MUSHPID;
	mb.msg_data.d_req = MM_SHARE;
	mb.msg_data.d_uid = uid;
	mb.msg_rply = 1;
	sendw(&mb, &mb, MSG_W_POST|MSG_W_RCV);
	return(mb.msg_data.d_fdat);
}
