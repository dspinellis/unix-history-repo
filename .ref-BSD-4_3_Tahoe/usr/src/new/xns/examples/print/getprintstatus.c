/* $Header: getprintstatus.c,v 2.1 87/01/14 13:12:50 ed Exp $ */

/*
 * $Log:	getprintstatus.c,v $
 * Revision 2.1  86/10/11  15:42:11  jqj
 * Some convenience changes from B Jackson @ PARC
 * 
 * Revision 2.0  85/11/21  07:23:10  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  13:56:47  jqj
 * Initial revision
 * 
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Printing3_defs.h"
#include <xnscourier/Clearinghouse2.h>
#include <xnscourier/except.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	GetPrinterStatusResults result;
	struct ns_addr *destaddr;
	CourierConnection *conn;
	char *xnshost;
	extern struct ns_addr *getXNSaddr();
	extern struct ns_addr *CH_LookupAddr();
	Clearinghouse2_Name hostname;
	Clearinghouse2_Name defaultname;
	extern Clearinghouse2_Name CH_StringToName();
	extern char *getenv();

	CH_NameDefault(&defaultname);
	xnshost = getenv("PRINTER");
	switch (argc) {
	case 2: xnshost = argv[1];
	case 1:	hostname = CH_StringToName(xnshost,&defaultname);
		if ((destaddr = CH_LookupAddr(hostname)) != NULL)
			break;		/* got a valid host name */
		else
			fprintf(stderr,"Invalid machine name: [%s:%s:%s]\n",
				hostname.object,
				hostname.domain,
				hostname.organization);
	default:
		fprintf(stderr,"Usage: %s [machine]\n",argv[0]);
		exit(1);
	}

	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",xnshost);
		exit(1);
	}

	DURING
		result = GetPrinterStatus(conn,NULL);
	HANDLER {
		switch (Exception.Code) {
		case ServiceUnavailable:
			fprintf(stderr,"Service unavailable\n");
			break;
		case SystemError:
			fprintf(stderr,"System Error\n");
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

	CourierClose(conn);
	printf("[%s:%s:%s]\n",
		hostname.object,
		hostname.domain,
		hostname.organization);
	printresults(result.status);
}

printresults(status)
	PrinterStatus status;
{
	int i, typ;
	static char *spoollist[] = {"available","busy","disabled","full"};
	static char *formatlist[] = {"available","busy","disabled"};
	static char *printlist[] = {"available","busy","disabled",
			"needs attention","needs key operator"};

	for (i = 0; i < status.length; i++) {
		switch (status.sequence[i].designator) {
		case spooler:
			typ = (int) status.sequence[i].spooler_case;
			printf("Spooling status:  %s\n", spoollist[typ]);
			break;
		case formatter:
			typ = (int) status.sequence[i].formatter_case;
			printf("Formatting status:  %s\n", formatlist[typ]);
			break;
		case printer:
			typ = (int) status.sequence[i].printer_case;
			printf("Printer status:  %s\n", printlist[typ]);
			break;
		case media:
			printmedia(status.sequence[i].media_case);
			break;
		}
	}
}

printmedia(media)
	Media media;
{
	int j;
	for (j = 0; j <media.length; j++) {
		switch (media.sequence[j].designator) {
		case paper:
			printf("Paper #%d:  ", j+1);
			printpaper(media.sequence[j].paper_case);
			break;
		default:
			printf("Unknown medium type\n");
			break;
		}
	}
}

printpaper(paper)
	Paper paper;
{
	switch (paper.designator) {
	case unknown:
		printf("unknown\n");
		break;
	case knownSize:
		switch (paper.knownSize_case) {
		case usLetter:
			printf("US letter\n");
			break;
		case usLegal:
			printf("US legal\n");
			break;
		default:
			printf("known size %d\n", paper.knownSize_case);
			break;
		}
		break;
	case otherSize:
		printf("\tother size: width = %dmm, length = %d mm\n",
			paper.otherSize_case.width,
			paper.otherSize_case.length);
		break;
	default:
		printf("Unknown paper type");
		break;
	}
}
