/*	@(#)setloglim.c	4.3	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/mu_msg.h>
#include <sys/quota.h>
#include <mushmuck.h>

setloglim(uid, limit, reason)
long limit;
{
	mmsgbuf	mb;

	mb.msg_pid = MUSHPID;
	mb.msg_data.d_req = MM_SLTIME;
	mb.msg_data.d_uid = uid;
	mb.msg_rply = 0;
	mb.msg_data.d_ldat = limit;
	mb.msg_data.d_info = reason;
	send(&mb, MSG_W_POST);
}
