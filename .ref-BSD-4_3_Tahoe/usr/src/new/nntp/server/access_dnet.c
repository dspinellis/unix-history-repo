#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "../common/conf.h"

#ifdef DECNET

#ifndef lint
static	char	*sccsid = "@(#)access_dnet.c	1.4	(Berkeley) 10/22/87";
#endif


/*
 * dnet_netnames -- return the network, subnet, and host names of
 * our peer process for the DECnet domain.  Since DECnet doesn't
 * have subnets, we always return "subnet_name"'s first char as '\0';
 *
 *	Parameters:	"sock" is the socket connect to our peer.
 *			"sap" is a pointer to the result of
 *			a getpeername() call.
 *			"net_name", "subnet_name", and "host_name"
 *			are filled in by this routine with the
 *			corresponding ASCII names of our peer.
 *	Returns:	Nothing.
 *	Side effects:	None.
 */

dnet_netnames(sock, sap, net_name, subnet_name, host_name)
	int		sock;
	struct sockaddr	*sap;
	char		*net_name;
	char		*subnet_name;
	char		*host_name;
{
	char		*cp;
	struct linger	l;
	char		*getenv();

	cp = getenv("NETWORK");
	(void) strcpy(net_name, cp ? cp : "DECnet");

	cp = getenv("REMNODE");
	(void) strcpy(host_name, cp ? cp : "unknown");

	*subnet_name = '\0';

	/*
	 * Give decnet a chance to flush its buffers before the
 	 * link is killed.
	 */

	l.l_onoff = 1;		/* on */
	l.l_linger = 15;	/* seconds */

	if (setsockopt(sock, SOL_SOCKET, SO_LINGER, (char *) &l,
		sizeof (l)) < 0) {
#ifdef LOG
		syslog(LOG_ERR,
			"access_dnet: setsockopt SOL_SOCKET SO_LINGER: %m");
#endif
	}
}
#endif
