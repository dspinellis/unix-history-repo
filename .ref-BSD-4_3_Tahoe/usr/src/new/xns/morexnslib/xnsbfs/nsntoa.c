/* $Header: nsntoa.c,v 1.1 86/06/27 13:14:36 jqj Exp $ */
/* $Log:	nsntoa.c,v $
 * Revision 1.1  86/06/27  13:14:36  jqj
 * Initial revision
 * 
 */
 
#include <sys/types.h>
#include <netns/ns.h>

/*
 * Convert network-format ns address to ascii.
 * This routine should be part of library, but just incase it isn't we
 * have it here.
 */
char *
ns_ntoa(addr)
	struct ns_addr addr;
{
	static char b[25];
	char temp[15];
	char *t;
	union ns_net_u net_u;


	/* net */
	net_u.net_e = addr.x_net;

	/* build a host number */
	sprintf(temp,"%02X%02X%02X%02X%02X%02X",
		addr.x_host.c_host[0],
		addr.x_host.c_host[1],
		addr.x_host.c_host[2],
		addr.x_host.c_host[3],
		addr.x_host.c_host[4],
		addr.x_host.c_host[5]);
	/* strip leading zeros */
	for (t = temp; *t == '0'; t++)
		;
	sprintf(b, "%lXH.%sH", ntohl(net_u.long_e),t);
	return (b);
}

