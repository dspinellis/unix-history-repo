/*	@(#)mu_msg.c	4.1	(Melbourne)	82/01/04	*/

#include <sys/types.h>
#include <sys/mu_msg.h>

mu_msg(a, b, c)
{
	return(syscall(64+62, a, b, c));
}
