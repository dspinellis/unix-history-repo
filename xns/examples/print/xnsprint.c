/*
 * $Header: xnsprint.c,v 2.4 87/01/09 16:34:23 ed Exp $
 *
 * a program to print InterPress masters on an InterPress printer via
 * Ethernet.   Uses xns Courier.
 * This version runs on 4.3BSD only!
 */

/*
 * $Log:	xnsprint.c,v $
 * Revision 2.4  87/01/09  16:34:23  ed
 * Webster version
 * 
 * Revision 2.4  87/01/09  16:34:23  ed
 * Added -W flag to wait until printing is completed (from Lee Moore)
 * 
 * Revision 2.3  86/12/11  04:43:34  jqj
 * Added support for -s and -m flags.
 * 
 * Revision 2.2  86/09/07  06:57:41  jqj
 * Fixed inconsistent calls to attnmsg() by eliminating stderr first arg.
 * Changed banner not to use "Cornell Computer Science".
 * 
 * Revision 2.1  86/05/16  11:04:18  jqj
 * fix to correspond to new enumeration semantics (tags are now global)
 * 
 * Revision 2.0  85/11/21  07:23:11  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  13:56:53  jqj
 * Initial revision
 * 
 * modified 8-6-85 by jqj.
 *  Eliminated any hardwired addresses.  Instead, use CH_Enumerate to
 *  find a printer if none is specified.  Also, you can now print multiple
 *  files in a single call to xnsprint, and getopt() is used to parse
 *  arguments.
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "Printing3_defs.h"
#include <xnscourier/Clearinghouse2.h>
#include <xnscourier/except.h>
#include <pwd.h>
#include <sys/file.h>
#include <strings.h>

static FILE *ipfile = NULL;
static int ExitStatus = 0;		/* modified lpd conventions: */
	/* 0 => Job printed.  (successfully sent to print-server) */
#define X_GOOD 0
	/* 1 => Couldn't send job.  Retry forever, should go eventually. */
#define X_RETRY 1
	/* 2 => Couldn't send job,  Strange error, Retry a limited number*/
	/*		of times.  If it still hasn't worked, give up.	 */
#define X_LIMRETRY 2
	/* 3 => Couldn't send job:  Hard error, don't bother retrying,	 */
	/*		get rid of the job.				 */
#define X_NORETRY 3

static struct {
	char * sizename;
	int sizevalue;
} papersizetable[] = {
	"usLetter", (int) usLetter, /* 1 */
	"usLegal", (int) usLegal, /* 2 */
	"a0", (int) a0, /* 3 */
	"a1", (int) a1, /* 4 */
	"a2", (int) a2, /* 5 */
	"a3", (int) a3, /* 6 */
	"a4", (int) a4, /* 7 */
	"a5", (int) a5, /* 8 */
	"a6", (int) a6, /* 9 */
	"a7", (int) a7, /* 10 */
	"a8", (int) a8, /* 11 */
	"a9", (int) a9, /* 12 */
	"a10", (int) a10, /* 35 */
	"isoB0", (int) isoB0, /* 13 */
	"isoB1", (int) isoB1, /* 14 */
	"isoB2", (int) isoB2, /* 15 */
	"isoB3", (int) isoB3, /* 16 */
	"isoB4", (int) isoB4, /* 17 */
	"isoB5", (int) isoB5, /* 18 */
	"isoB6", (int) isoB6, /* 19 */
	"isoB7", (int) isoB7, /* 20 */
	"isoB8", (int) isoB8, /* 21 */
	"isoB9", (int) isoB9, /* 22 */
	"isoB10", (int) isoB10, /* 23 */
	"jisB0", (int) jisB0, /* 24 */
	"jisB1", (int) jisB1, /* 25 */
	"jisB2", (int) jisB2, /* 26 */
	"jisB3", (int) jisB3, /* 27 */
	"jisB4", (int) jisB4, /* 28 */
	"jisB5", (int) jisB5, /* 29 */
	"jisB6", (int) jisB6, /* 30 */
	"jisB7", (int) jisB7, /* 31 */
	"jisB8", (int) jisB8, /* 32 */
	"jisB9", (int) jisB9, /* 33 */
	"jisB10", (int) jisB10, /* 34 */
	(char *) 0, 0
};

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
/* 
 * misc externals
 */
int remove = 0;
int quiet = 0;
int attn = 0;		/* Write lpr system STATUS file?	LCP 850415*/
char *attnfile;		/* Status file name.		LCP 850415 */
char *FileName = NULL;
char *UserName = NULL;
char *Banner = NULL;
int copies = 1;
Medium paperchoice;
char *UserMessage = NULL;
Clearinghouse2_Name hostname;
char *xnshost = NULL;
int WaitFlag = 0;		/* wait until job is printed ? LCM */

setxnshost(name)
	Clearinghouse2_ObjectName name;
{
	extern char *malloc(), *strcpy();

	if (xnshost == NULL)
	  xnshost = strcpy(malloc(strlen(name.object)+1),name.object);
}

main(argc, argv)
	int argc;
	char **argv;
{
	struct ns_addr *destaddr;
	CourierConnection *conn;
	extern struct ns_addr *getXNSaddr();
	extern struct ns_addr *CH_LookupAddr();
	Clearinghouse2_Name defaultname;
	extern Clearinghouse2_Name CH_StringToName();
	int opt;
	extern int optind;
	extern char *optarg;
	int errflg = 0;
	int i;

	paperchoice.designator = paper;
	paperchoice.paper_case.designator = knownSize;
	paperchoice.paper_case.knownSize_case = usLetter;

	while ((opt = getopt(argc,argv,"c:n:b:P:h:rqa:lm:s:W")) != EOF)
	    switch (opt) {
	    case 'c':	/* copies */
		copies = atoi(optarg);
		break;
	    case 'n':	/* user name */
		UserName = optarg;
		break;
	    case 'b':   /* file name */
		Banner = optarg;
		break;
	    case 'P':	/* printer */
	    case 'h':   /* host */
		xnshost = optarg;
		break;
	    case 'r':	/* remove input file when done */
		remove++;
		break;
	    case 'q':	/* don't print status messages */
		quiet++;
		break;
	    case 'a':	/* Write lpr STATUS file.  Name follows.  LCP 850415 */
		quiet++;
		attn++;
		attnfile = optarg;
		break;
	    case 'l':	/* use legal-sized (long) paper */
	 	paperchoice.paper_case.knownSize_case = usLegal;
		break;
	    case 'm':	/* message field follows (default to XNS name) */
		UserMessage = optarg;
		break;
	    case 's':	/* papersize name follows */
		for (i = 0; papersizetable[i].sizename != NULL; i++)
			if (strcmp(optarg,papersizetable[i].sizename) == 0) {
				*(int*)& paperchoice.paper_case.knownSize_case = 
					papersizetable[i].sizevalue;
				goto gotsize;
				}
		*(int*)& paperchoice.paper_case.knownSize_case = atoi(optarg);
	    gotsize:
		break;
	    case 'W':			/* wait for the job to be printed LCM */
		WaitFlag++;
		break;
	    default:
		errflg = 1;
	    }
	if (errflg) {
		attnmsg("Usage: %s [-r] [-P host] [-c #] [-n name] [-b banner] [-l] [-s size] [-m message] [-W] file...\n",
			argv[0]);
		exit(X_NORETRY);
	}

	/* set User Name for banner if necessary */
	if (UserName == NULL) {
	    struct passwd *pwd, *getpwuid();
	    char *p;
	    extern char *getenv(), *index();

	    UserName = getenv("USER");
	    if ((pwd = getpwuid(getuid())) != NULL) {
		UserName = pwd->pw_gecos;
		if (p = index(UserName,','))
			*p = '\000';
	    }
	}

	/* figure out what address we're sending to */
	CH_NameDefault(&defaultname);/* default from clearinghouse.addresses */
	if (xnshost == NULL) {
		xnshost= getenv("PRINTER");
		if ( (xnshost == NULL) || (*xnshost == '\0') ) {
			/* find the first object in the local domain of the CH 
			 * with a printService property.  setxnshost sets xnshost
			 * to the name part of the object
			 */
			hostname = defaultname;
			hostname.object = "*";
			CH_Enumerate(hostname,10001,setxnshost);
			hostname.object = xnshost;
		} else
			hostname = CH_StringToName(xnshost,&defaultname);
	}
	else hostname = CH_StringToName(xnshost,&defaultname);

	if ((destaddr = CH_LookupAddr(hostname,4)) == NULL) {
		attnmsg("Invalid address, %s:%s:%s\n",
			hostname.object,hostname.domain,hostname.organization);
		exit(X_NORETRY);
	}

	/* make sure the printer is available */
	checkIPstatus(destaddr);

	for ( ; optind < argc; optind++) {
	    FileName = argv[optind];
	    if (strcmp(FileName,"-") == 0) {
		ipfile = stdin;
		FileName = "standard input";
	    }
	    else if ((ipfile = fopen(FileName,"r")) == NULL) {
		fprintf(stderr, "%s: Can't open %s\n", argv[0], FileName);
		exit(X_NORETRY);
	    }
	    if(Banner == NULL)
		Banner = FileName;

	    if (!quiet)
		printf("Sending to %s...", xnshost);
	    fflush(stdout);

	    sendIPfile(ipfile,destaddr);
	    if (ipfile != stdin)
		fclose(ipfile);
	}

	if (!quiet)
		printf("Done.\n");
	exit(X_GOOD);
}

/*
 * Check printer status first so we won't dump big interpress
 * files accross the net unless we're fairly confidant that they'll
 * be accepted.
 */
checkIPstatus(destaddr)
	struct ns_addr *destaddr;
{
	CourierConnection *conn;
	GetPrinterStatusResults StatusResult;

	do {
	    if (!quiet)
		printf("Opening connection to %s. ",xnshost);
	    if (attn)
		attnmsg("Opening connection to %s.\n",xnshost);
	    if ((conn = CourierOpen(destaddr)) == NULL) {
		attnmsg("Can't open connection to %s\n",xnshost);
		if(remove && !attn)
		    attnmsg("Output left in %s\n", FileName);
		exit(X_LIMRETRY);
	    }
	    if (!quiet)
		printf("Connected.\n");
	    if (attn)
		attnmsg("Requesting status.\n");
	    DURING
		StatusResult = GetPrinterStatus(conn,NULL);
	    HANDLER {
		ExitStatus = X_LIMRETRY;
		switch (Exception.Code) {
		case ServiceUnavailable:
			attnmsg("GetStat: Service unavailable\n");
			ExitStatus = X_NORETRY;
			break;
		case SystemError:
			attnmsg("GetStat: System Error\n");
			break;
		case Undefined:
			attnmsg("GetStat: Undefined error, number %d\n",
				CourierErrArgs(UndefinedArgs,problem));
			break;
		case REJECT_ERROR:
			attnmsg("GetStat: REJECT:  type = %d\n",
				CourierErrArgs(rejectionDetails, designator));
			break;
		default:
			attnmsg("GetStat: Some random error, code %d\n",
				Exception.Code);
			break;
		}
	    if (remove && !attn) 
		attnmsg("Output left in %s\n", FileName);
	    exit(ExitStatus);
	    } END_HANDLER;

	    CourierClose(conn);
	} while (printresults(StatusResult.status) != 0);
}

/* 
 * display printer status, return 0 IFF spooler is available 
 */
int
printresults(status)
	PrinterStatus status;
{
	int i, typ;
	static char *spoollist[] = {"available","busy","disabled","full"};
	static char *formatlist[] = {"available","busy","disabled"};
	static char *printlist[] = {"available","busy","disabled",
			"needs attention","needs key operator"};
	int error = 1;
	char bufr[256];

	bufr[0] = '\0';
	for (i = 0; i < status.length; i++) {
		switch (status.sequence[i].designator) {
		case spooler:
			typ = (int) status.sequence[i].spooler_case;
			if (!quiet || typ > 1)
			    sprintf(bufr+strlen(bufr),
				"Spooler: %s; ", spoollist[typ]);
			error = typ;
			break;
		case formatter:
			typ = (int) status.sequence[i].formatter_case;
			if (!quiet || typ > 1)
			    sprintf(bufr+strlen(bufr),
				"Formatter: %s; ", formatlist[typ]);
			break;
		case printer:
			typ = (int) status.sequence[i].printer_case;
			if (!quiet || typ > 1)
			    sprintf(bufr+strlen(bufr),
				"Printer: %s. ", printlist[typ]);
			break;
		case media:
			/* printmedia(status.sequence[i].media_case); */
			break;
		}
	}
	if (bufr[0] != '\0')
	{
	    if (attn)
		attnmsg("%s\n",bufr);
	    else
	        printf("%s\n",bufr);
	}

	switch(error) {
		case 0:
			break;
		case 1:
			if (!quiet)
			    printf("Retrying... ");
			if (bufr[0] != '\0' && attn)
			    attnmsg("Status: Busy.  Retrying...\n");
			fflush(stdout);
			sleep(15);
			break;
		default:
			if(remove && !attn)
			    attnmsg( "Output left in %s\n", FileName);
			exit(1);
	}
	return(error);
}


attnmsg(fmt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
	char *fmt;
{
	char bufr[256];
	int af;

	if (attn)
	{
	    if ((af = open(attnfile,O_TRUNC|O_WRONLY|O_CREAT,0666)) < 0)
		    return; /* Oh Well. */

	    sprintf(bufr,fmt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9);

	    (void) write(af,bufr,strlen(bufr));	/* In case of error??? */
	    close(af);
	}
	else
	    fprintf(stderr,fmt,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9);
}

sendIPfile(ipfile,destaddr)
	FILE *ipfile;
	struct ns_addr *destaddr;
{
	PrintResults result;
	CourierConnection *conn;
	PrintAttributes attributes;
	PrintOptions options;
	char *malloc();

	/* only use sender name and file name, no date */
	attributes.length = 2;
	attributes.sequence = malloc(attributes.length *
					sizeof(*attributes.sequence));
	attributes.sequence[0].designator = printObjectName;
	attributes.sequence[0].printObjectName_case = Banner;
	attributes.sequence[1].designator = senderName;
	attributes.sequence[1].senderName_case = UserName;

	options.length = 3;
	options.sequence = malloc(options.length *
					sizeof(*options.sequence));
	options.sequence[0].designator = copyCount;
	options.sequence[0].copyCount_case = copies;
	options.sequence[1].designator = mediumHint;
	options.sequence[1].mediumHint_case = paperchoice;
	options.sequence[2].designator = message;
	options.sequence[2].message_case =
		UserMessage ? UserMessage :
			      sprintf(malloc(44),"%s:%s:%s",
					hostname.object,hostname.domain,
					hostname.organization)
		;

again:
	if (!quiet)
		printf("Opening connection to %s. ",xnshost);
	if (attn)
		attnmsg("Opening connection to %s.\n",xnshost);

	if ((conn = CourierOpen(destaddr)) == NULL) {
		attnmsg("Can't open connection to %s\n",xnshost);
		if(remove && !attn)
		    attnmsg("Output left in %s\n", FileName);
		exit(X_LIMRETRY);
	}

	if (!quiet)
		printf("Connected.\n");
	if (attn)
		attnmsg("Sending to %s\n",xnshost);

	DURING
		result = Print(conn, SendSource, BulkData1_immediateSource,
					attributes, options);
	HANDLER {
		ExitStatus = X_RETRY;
		switch (Exception.Code) {
		case Busy:
			if (!quiet)
			    printf("Busy, retrying...\n");
			if (attn)
			    attnmsg("Busy, retrying...\n");
			CourierClose(conn);
			sleep(15);
			if (rewind(ipfile) < 0) {
				ExitStatus = X_LIMRETRY;
				attnmsg("Can't rewind file\n");
			}
			goto again;
		case ConnectionError:
			ExitStatus = X_LIMRETRY;
			attnmsg("Connection error, %d\n",
				CourierErrArgs(ConnectionErrorArgs,problem));
			break;
		case InsufficientSpoolSpace:
			attnmsg("Insufficient Spool Space error\n");
			break;
		case InvalidPrintParameters:
			ExitStatus = X_LIMRETRY;
			attnmsg("InvalidPrintParameters error\n");
			break;
		case MasterTooLarge:
			ExitStatus=X_NORETRY;
			attnmsg("MasterTooLarge error\n");
			break;
		case MediumUnavailable:
			ExitStatus=X_NORETRY;
			attnmsg("MediumUnavailable error\n");
			break;
		case ServiceUnavailable:
			ExitStatus=X_NORETRY;
			attnmsg("ServiceUnavailable error\n");
			break;
		case SpoolingDisabled:
			attnmsg("SpoolingDisabled\n");
			break;
		case SpoolingQueueFull:
			attnmsg("SpoolingQueueFull error\n");
			break;
		case SystemError:
			ExitStatus = X_LIMRETRY;
			attnmsg("System Error\n");
			break;
		case TooManyClients:
			attnmsg("TooManyClients error\n");
			break;
		case TransferError:
			ExitStatus = X_LIMRETRY;
			attnmsg("TransferError error\n");
			break;
		case Undefined:
			attnmsg("Undefined error, number %d\n",
				CourierErrArgs(UndefinedArgs,problem));
			break;
		case REJECT_ERROR:
			ExitStatus = X_LIMRETRY;
			attnmsg("REJECT:  type = %d\n",
				CourierErrArgs(rejectionDetails, designator));
			break;
		default:
			ExitStatus = X_LIMRETRY;
			attnmsg("Some random error, code %d\n",
				Exception.Code);
			break;
		}
		if(remove && !attn)
		    attnmsg("Output left in %s\n", FileName);
		exit(ExitStatus);
	} END_HANDLER;

	if (WaitFlag)			/* wait for completion LCM */
		WaitForCompletion(conn, result.printRequestID);
	CourierClose(conn);

	/* RETURNS [printRequestID: RequestID] */
	if(remove) unlink(FileName);
}	

/*
 * Wait for the job to complete
 */
WaitForCompletion(conn, printRequestID)
    CourierConnection *conn;
    Printing3_RequestID printRequestID;
{
	static char *statusStrings[] = {"pending", "inProgress", "completed",
		"completedWithWarning", "unknown", "rejected", "aborted",
		"canceled", "held"};
#define DONE	0
#define WAIT	1
	static char statusActions[] = {WAIT,      WAIT,         DONE,
		DONE,			DONE,	   DONE,       DONE,
		DONE,       WAIT};
	int i, typ,
		cycle,
		action;
	GetPrintRequestStatusResults result;

	for(cycle = 0;; cycle++) {
		if (!quiet)
			printf("try #%d\n", cycle);

		DURING
			result = GetPrintRequestStatus(conn, NULL, printRequestID);
		HANDLER {
			ExitStatus = X_NORETRY;	   /* if it got this far... */

			switch (Exception.Code) {
			case ServiceUnavailable:
				attnmsg("GetReqStat: Service unavailable\n");
				break;
			case SystemError:
				attnmsg("GetReqStat: System error\n");
				break;
			case Undefined:
				attnmsg("GetReqStat: Undefined error, number %d\n",
					CourierErrArgs(UndefinedArgs, problem));

			case REJECT_ERROR:
				attnmsg("GetReqStat: REJECT: type = %d\n",
					CourierErrArgs(rejectionDetails, designator));
				break;
			default:
				attnmsg("GetStat: Some random error, code %d\n",
					Exception.Code);
				break;
			}

		exit(ExitStatus);
		} END_HANDLER;

		action = WAIT;

		/* check out the returned status */
		for( i = 0; i < result.status.length; i++ ) {
			switch (result.status.sequence[i].designator) {
			case status:
				typ = (int) result.status.sequence[i].status_case;
				action = statusActions[typ];

				if (!quiet)
					printf("\tstatus: %s\n", statusStrings[typ]);

				break;

			case statusMessage:
				if(!quiet)
					printf("\tstatus message (%d bytes): %s\n",
						strlen(result.status.sequence[i].statusMessage_case),
						result.status.sequence[i].statusMessage_case);
				break;

			default:
				printf("GetReqStatu: help!\n");
			}
		}

		if( action == DONE )
			return;

		sleep(3);	/* wait three seconds before trying again */
	}
}
