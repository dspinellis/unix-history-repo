#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Printing3_defs.h"
#include <xnscourier/except.h>

static FILE *ipfile = NULL;

SendSource(bdtconnection)
CourierConnection *bdtconnection;
{
	int count;
	char buffer[SPPMAXDATA];

	while ( (count = fread(buffer,1,SPPMAXDATA,ipfile)) > 0 &&
		BDTwrite(bdtconnection,buffer,count) >= 0 )
		;
	if (count <= 0)
		BDTclosewrite(bdtconnection);	/* last packet with EOM set */
	else
		BDTabort(bdtconnection);
}

main(argc, argv)
	int argc;
	char *argv[];
{
	PrintResults result;
	struct xn_addr *destaddr;
	CourierConnection *conn;
	extern struct xn_addr *getXNSaddr();
	char *xnshost;
	PrintAttributes attributes;
	PrintOptions options;
	xnshost = "2-273#2-852-159-207";	/* CornellS2 */

		/* default to CornellS1 2-273, 2-852-151-014 */
		/* xnshost = "8E1#00.00.AA.00.5E.E6"; */

	if ((destaddr = getXNSaddr(xnshost)) == NULL) {
		fprintf(stderr,"Invalid machine name.\n");
	}

	switch (argc) {
	case 2: if ((ipfile = fopen(argv[1],"r")) != NULL)
			break;		/* got a valid file name */
	default:
		fprintf(stderr,"Usage: %s file\n",argv[0]);
		exit(1);
	}

	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",xnshost);
		exit(1);
	}

	attributes.length = 0;
	options.length = 0;

	DURING
		result = Print(conn, SendSource, BulkData1_immediateSource,
					attributes, options);
	HANDLER {
		switch (Exception.Code) {
		case Busy:
			fprintf(stderr,"Busy\n");
			break;
		case ConnectionError:
			fprintf(stderr,"Connection error, %d\n",
				CourierErrArgs(ConnectionErrorArgs,problem));
			break;
		case InsufficientSpoolSpace:
			fprintf(stderr,"Insufficient Spool Space error\n");
			break;
		case InvalidPrintParameters:
			fprintf(stderr,"InvalidPrintParameters error\n");
			break;
		case MasterTooLarge:
			fprintf(stderr,"MasterTooLarge error\n");
			break;
		case MediumUnavailable:
			fprintf(stderr,"MediumUnavailable error\n");
			break;
		case ServiceUnavailable:
			fprintf(stderr,"ServiceUnavailable error\n");
			break;
		case SpoolingDisabled:
			fprintf(stderr,"SpoolingDisabled\n");
			break;
		case SpoolingQueueFull:
			fprintf(stderr,"SpoolingQueueFull error\n");
			break;
		case SystemError:
			fprintf(stderr,"System Error\n");
			break;
		case TooManyClients:
			fprintf(stderr,"TooManyClients error\n");
			break;
		case TransferError:
			fprintf(stderr,"TransferError error\n");
			break;
		case Undefined:
			fprintf(stderr,"Undefined error, number %d\n",
				CourierErrArgs(UndefinedArgs,problem));

		case REJECT_ERROR:
			fprintf(stderr,"REJECT:  type = %d\n",
				CourierErrArgs(rejectionDetails, designator));
			break;
		default:
			fprintf(stderr,"Some random error, code %d\n",
				Exception.Code);
			break;
		}
	exit(1);
	} END_HANDLER;

	/* CourierClose(conn); */
	/* RETURNS [printRequestID: RequestID] */
	printf("Done.  Request ID %x %x %x %x %x\n",
		result.printRequestID[0],
		result.printRequestID[1],
		result.printRequestID[2],
		result.printRequestID[3],
		result.printRequestID[4]);
}
