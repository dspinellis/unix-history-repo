/*
 * This is a short demo program that queries the Clearinghouse for the
 * NS Address of a specified host.
 * It should be rewritten to format the network address more reasonably.
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/courier.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	ObjectName name, defaults;
	struct ns_addr *result;
	extern struct ns_addr *CH_LookupAddr();
	extern ObjectName CH_StringToName();
	char *XNSaddrToString();

	if (argc != 2) {
		fprintf(stderr,"Usage: %s name\n",argv[0]);
		exit(1);
	}
	CH_NameDefault(&defaults);
	name = CH_StringToName(argv[1],&defaults);
	result = CH_LookupAddr(name,4);
	if (result==0) {
		printf("%s not found\n",name.object);
		exit(1);
	}
	printf("name: %s:%s:%s\n",name.object,name.domain,name.organization);
	printf("address: %s\n",	XNSaddrToString(result));
}

char *
XNSaddrToString(addr)
	struct ns_addr *addr;
{
	u_char *s;
	static char buf[21];
	union {
		u_short y_net[2];
		u_long y_long;
	} netvalue;

	s = addr->x_host.c_host;
	netvalue.y_net[0] = addr->x_net.s_net[0];
	netvalue.y_net[1] = addr->x_net.s_net[1];
	sprintf(buf,"%lx#%x.%x.%x.%x.%x.%x#%x",
		ntohl(netvalue.y_long),
		s[0], s[1], s[2], s[3], s[4], s[5],
		ntohs(addr->x_port));
	return(buf);
}
