
/*	@(#)mushlset.c	4.2	(Melbourne)	82/05/30	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/mu_msg.h>
#include <sys/quota.h>
#include <mushmuck.h>

mushlset(uid, req, dat)
long dat;
{
	mmsgbuf	mb;

	mb.msg_pid = MUSHPID;
	mb.msg_data.d_req = MM_ALTER;
	mb.msg_data.d_info = req;
	mb.msg_data.d_uid = uid;
	mb.msg_data.d_ldat = dat;
	mb.msg_rply = 0;
	send(&mb, MSG_W_POST);
}
