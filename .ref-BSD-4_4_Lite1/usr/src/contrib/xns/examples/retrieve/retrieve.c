#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/except.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	RetrieveAddressesResults result;
	struct ns_addr *destaddr;
	CourierConnection *conn;
	NetworkAddress na;
	extern struct ns_addr *getXNSaddr();
	int i;

	if (argc == 1)
		/* default to CornellS1 2-273, 2-852-151-014 */
		destaddr = getXNSaddr("8E0#00.00.AA.00.7D.E7");
	else if (argc != 2 || (destaddr = getXNSaddr(argv[1]))==NULL) {
		fprintf(stderr,"Usage: %s machine\n",argv[0]);
		exit(1);
	}
	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",argv[1]);
		exit(1);
	}
	DURING
		result = RetrieveAddresses(conn,NULL);
	HANDLER {
		switch (Exception.Code) {
		case CallError:
			fprintf(stderr,"Call error, %d\n",
				CourierErrArgs(CallErrorArgs,problem));
			break;
		default:
			fprintf(stderr,"Some random error, code %d\n",
				Exception.Code);
			break;
		}
	exit(1);
	} END_HANDLER;

	for (i = 0; i < result.addresses.length; i++) {
		na = result.addresses.sequence[i];
		printf("address:  network %x %x, host %x, %x, %x, socket %x\n",
			na.network[0],na.network[1],
			na.host[0],na.host[1],na.host[2],
			na.socket);
	}
}
