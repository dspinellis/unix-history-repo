/*	if.c	*/

/* EGP User Process, ISI 23-Jun-84 */

#include "include.h"


/*
 * Find the interface on the network of the specified address.
 */
struct interface *
if_withnet(addr)
	register struct sockaddr_in *addr;
{
	register struct interface *ifp;

	if (addr->sin_family != AF_INET)
		return (0);
	for (ifp = ifnet; ifp; ifp = ifp->int_next)
		if( inet_netof( addr->sin_addr)
		== inet_netof( in_addr_ofs( &ifp->int_addr)))
			break;
	return( ifp);
}


/* if_check() sets the current status of all interfaces
 * Returns 1 if any interface has changed status, 0 otherwise
 */

if_check(){
	register struct interface *ifp;
	struct ifreq ifrequest;
	int  if_change = FALSE;
		
	for(ifp=ifnet; ifp!=NULL; ifp=ifp->int_next) {

				/* get interface status flags */
		strcpy(ifrequest.ifr_name, ifp->int_name);
		if(ioctl(ifp->int_egpsock, SIOCGIFFLAGS,
						 (char *)&ifrequest))
		    p_error("mknrnets:ioctl SIOCGIFFALGS");
				/* check change in interface status */
		else if( (ifrequest.ifr_flags & IFF_UP)
				  != (ifp->int_flags & IFF_UP) ) {
			if_change = TRUE;
			if( ifrequest.ifr_flags & IFF_UP)
				ifp->int_flags |= IFF_UP;
			else
				ifp->int_flags &= ~IFF_UP;
		}
	}
	return( if_change);
}
