#ifndef lint
static char	*sccsid = "@(#)access_inet.c	1.2	(Berkeley) 10/15/87";
#endif

#include <stdio.h>
#include <strings.h>
#include <netdb.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "../common/conf.h"

/*
 * inet_netnames -- return the network, subnet, and host names of
 * our peer process for the Internet domain.
 *
 *	Parameters:	"sock" is our socket, which we don't need.
 *			"sin" is a pointer to the result of
 *			a getpeername() call.
 *			"net_name", "subnet_name", and "host_name"
 *			are filled in by this routine with the
 *			corresponding ASCII names of our peer.
 *	Returns:	Nothing.
 *	Side effects:	None.
 */

inet_netnames(sock, sin, net_name, subnet_name, host_name)
	int			sock;
	struct sockaddr_in	*sin;
	char			*net_name;
	char			*subnet_name;
	char			*host_name;
{
	static int		gotsubnetconf;
	u_long			net_addr;
	u_long			subnet_addr;
	struct hostent		*hp;
	struct netent		*np;

#ifdef SUBNET
	if (!gotsubnetconf) {
		if (getifconf() < 0) {
#ifdef SYSLOG
			syslog(LOG_ERR, "host_access: getifconf: %m");
#endif
			return;
		}
		gotsubnetconf = 1;
	}
#endif

	net_addr = inet_netof(sin->sin_addr);	/* net_addr in host order */
	np = getnetbyaddr(net_addr, AF_INET);
	if (np != NULL)
		(void) strcpy(net_name, np->n_name);
	else
		(void) strcpy(net_name,inet_ntoa(*(struct in_addr *)&net_addr));

#ifdef SUBNET
	subnet_addr = inet_snetof(sin->sin_addr.s_addr);
	if (subnet_addr == 0)
		subnet_name[0] = '\0';
	else {
		np = getnetbyaddr(subnet_addr, AF_INET);
		if (np != NULL)
			(void) strcpy(subnet_name, np->n_name);
		else
			(void) strcpy(subnet_name,
			    inet_ntoa(*(struct in_addr *)&subnet_addr));
	}
#else
	subnet_name[0] = '\0';
#endif SUBNET

	hp = gethostbyaddr((char *) &sin->sin_addr.s_addr,
		sizeof (sin->sin_addr.s_addr), AF_INET);
	if (hp != NULL)
		(void) strcpy(host_name, hp->h_name);
	else
		(void) strcpy(host_name, inet_ntoa(sin->sin_addr));
}
