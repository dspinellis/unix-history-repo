/*	@(#)reply.c	4.1	(Melbourne)	82/01/04	*/

#include <sys/types.h>
#include <sys/mu_msg.h>

reply(mp, opt)
{
	return(mu_msg(MSG_RPLY, opt, mp));
}
