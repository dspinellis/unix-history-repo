/*	@(#)recv.c	4.1	(Melbourne)	82/01/04	*/

#include <sys/types.h>
#include <sys/mu_msg.h>

recv(mp, opt)
{
	return(mu_msg(MSG_RECV, opt, mp));
}
