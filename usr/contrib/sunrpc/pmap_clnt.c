/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#ifndef lint
static char sccsid[] = "@(#)pmap_clnt.c 1.4 85/04/08 Copyr 1984 Sun Micro";
#endif

/*
 * pmap_clnt.c
 * Client interface to pmap rpc service.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include "types.h"
#include <netinet/in.h>
#include "xdr.h"
#include "auth.h"
#include "clnt.h"
#include "rpc_msg.h"
#include "pmap_prot.h" 
#include "pmap_clnt.h"
#include <sys/socket.h>
#include <sys/time.h>
#include <stdio.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#define NAMELEN 255

static struct timeval timeout = { 5, 0 };
static struct timeval tottimeout = { 60, 0 };
static struct sockaddr_in myaddress;

void clnt_perror();


/*
 * Set a mapping between program,version and port.
 * Calls the pmap service remotely to do the mapping.
 */
bool_t
pmap_set(program, version, protocol, port)
	u_long program;
	u_long version;
	u_long protocol;
	u_short port;
{
	struct sockaddr_in myaddress;
	int socket = -1;
	register CLIENT *client;
	struct pmap parms;
	bool_t rslt;

	get_myaddress(&myaddress);
	client = clntudp_create(&myaddress, PMAPPROG, PMAPVERS,
	    timeout, &socket);
	if (client == (CLIENT *)NULL)
		return (FALSE);
	parms.pm_prog = program;
	parms.pm_vers = version;
	parms.pm_prot = protocol;
	parms.pm_port = port;
	if (CLNT_CALL(client, PMAPPROC_SET, xdr_pmap, &parms, xdr_bool, &rslt,
	    tottimeout) != RPC_SUCCESS) {
		clnt_perror(client, "Cannot register service");
		return (FALSE);
	}
	CLNT_DESTROY(client);
	(void)close(socket);
	return (rslt);
}

/*
 * Remove the mapping between program,version and port.
 * Calls the pmap service remotely to do the un-mapping.
 */
bool_t
pmap_unset(program, version)
	u_long program;
	u_long version;
{
	struct sockaddr_in myaddress;
	int socket = -1;
	register CLIENT *client;
	struct pmap parms;
	bool_t rslt;

	get_myaddress(&myaddress);
	client = clntudp_create(&myaddress, PMAPPROG, PMAPVERS,
	    timeout, &socket);
	if (client == (CLIENT *)NULL)
		return (FALSE);
	parms.pm_prog = program;
	parms.pm_vers = version;
	parms.pm_port = parms.pm_prot = 0;
	CLNT_CALL(client, PMAPPROC_UNSET, xdr_pmap, &parms, xdr_bool, &rslt,
	    tottimeout);
	CLNT_DESTROY(client);
	(void)close(socket);
	return (rslt);
}

/* 
 * don't use gethostbyname, which would invoke yellow pages
 */
get_myaddress(addr)
	struct sockaddr_in *addr;
{
	int s;
	char buf[BUFSIZ];
	struct ifconf ifc;
	struct ifreq ifreq, *ifr;
	int len;

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	    perror("get_myaddress: socket");
	    exit(1);
	}
	ifc.ifc_len = sizeof (buf);
	ifc.ifc_buf = buf;
	if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
		perror("get_myaddress: ioctl (get interface configuration)");
		exit(1);
	}
	ifr = ifc.ifc_req;
	for (len = ifc.ifc_len; len; len -= sizeof ifreq) {
		ifreq = *ifr;
		if (ioctl(s, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
			perror("get_myaddress: ioctl");
			exit(1);
		}
		if ((ifreq.ifr_flags & IFF_UP) &&
		    ifr->ifr_addr.sa_family == AF_INET) {
			*addr = *((struct sockaddr_in *)&ifr->ifr_addr);
			addr->sin_port = htons(PMAPPORT);
			break;
		}
		ifr++;
	}
	close(s);
}
