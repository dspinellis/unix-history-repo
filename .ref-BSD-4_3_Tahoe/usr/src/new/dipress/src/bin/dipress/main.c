/******************************************************************************
 *	(C) Copyright 1985, 1986 Xerox Corporation
 *
 *	main	--	main routine for dipress 
 *			(ditroff to interpress conversion)
 *
 *	John Mellor-Crummey (Xerox Corp)
 *
 *
 * HISTORY
 * 07-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Converted to use getopt.
 *
 *
 *****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <strings.h>

#include "defs.h"	/* constant and macro definitions */
#include "externs.h"	/* declarations for global variables */

#define SAME	0	/* equality comparison for strings */

enum IPDeviceType IPDeviceType = GenericIPDevice;

/*-----------------------------------------------------------------------------
 *
 *	main processes the list of command line options and opens the 
 *	necessary temporary files
 *
 *---------------------------------------------------------------------------*/
main(argc, argv)
int argc;
char **argv;
{
	FILE *inputfile;
	int c;
	extern int optind;
	extern char *optarg;

	/* set up signal handlers to insure clean termination */
	signalHandler();	

	/* process command line options */

	while ((c = getopt(argc, argv, "d:D:f:F:o:t")) != EOF)
		switch (c) 
		{
		case 'd':		
			/* produce debugging info */
			dbg = atoi(optarg);
			if (dbg == 0) dbg = 1;
			setbuf(stdout, (char *)NULL);
			printf("debugging level %d\n", dbg);
			break;
		case 'D':
			/* read the following list of page numbers */
			setIPDeviceType(optarg);
			break;
		case 'f':
		case 'F':
			/* reset default font directory */
			fontdirectory = optarg;
			break;
		case 'o':		
			/* read the following list of page numbers */
			readPageList(optarg);
			break;
		case 't':		
			/* dump the interpress to the standard output */
			outputfile = fileno(stdout);
			break;
		}

	/* construct name for temporary files */
	(void) sprintf(tempfilename, "/tmp/dip%d", getpid());

	if (outputfile != fileno(stdout))
	{
		/* open file to hold master */
		if ((outputfile=open(tempfilename, O_WRONLY | O_TRUNC | O_CREAT, 0666)) == -1)
			reportError(QUIT, "temporary file %s: %s",
			tempfilename, sys_errlist[errno]);
		ip_select(outputfile);
	}

	/* open file to hold body */
	(void) strcat(tempfilename, "b");

	if ((pagebodyfile = open(tempfilename, O_WRONLY | O_TRUNC | O_CREAT, 0666)) == -1)
		reportError(QUIT, "temporary file %s: %s",
		tempfilename, sys_errlist[errno]);

	ip_raw_select(pagebodyfile);



	if (argc - optind == 0)	/* if no file specified, default to stdin */
		ditroffToIpress(stdin);
	else
		/* process files explicitly specified */
		for (; argc > optind; optind++)
		{
			if ((inputfile = fopen(argv[optind], "r")) == NULL)
				reportError(QUIT, "can't open %s: %s",
					argv[optind], sys_errlist[errno]);
			ditroffToIpress(inputfile);
			(void) fclose(inputfile);
		}
	goodbye();
}


/*-----------------------------------------------------------------------------
 * readPageList	--	read a list of page specifications of the form
 *			p1 or p1-p2 separated by commas. if in the dash form
 *			either p1 or p2 is omitted, it will appropriately 
 *			default to the first of last page of the document
 *---------------------------------------------------------------------------*/
readPageList(ptr)
char *ptr;
{
	while(*ptr != '\0')
	{
		pagerange[nPageRanges][0] = 
			((isdigit(*ptr)) ? readPageNumber(&ptr): DEFAULTRANGEBOT);
		pagerange[nPageRanges][1] = 
			((*ptr != '-') ? pagerange[nPageRanges][0] : 
					((isdigit(*++ptr)) ? readPageNumber(&ptr) : 
							 DEFAULTRANGETOP));
		nPageRanges++;
		if (*ptr != '\0') ptr++;
	}
}


/*-----------------------------------------------------------------------------
 * readPageNumber	--	read a page number from a string and update the
 *				pointer to the next non-digit
 *---------------------------------------------------------------------------*/
readPageNumber(ptr)
char **ptr;
{
	int pagenumber = 0;

	while(isdigit(**ptr))
	{
		pagenumber = pagenumber * 10 + **ptr - '0';
		++*ptr;
	}
	return(pagenumber);
}

/*-----------------------------------------------------------------------------
 * setIPDeviceType	--	record the type of the Interpress output device.
 *				This must be done because of limitations in
 *				some devices.
 *---------------------------------------------------------------------------*/
setIPDeviceType(deviceString)
char *deviceString;
{
	if( strcmp(deviceString, "Xerox8044_Services8") == SAME )
		IPDeviceType = Xerox8044_Services8;
	else if( strcmp(deviceString, "Xerox8044_Services9") == SAME )
		IPDeviceType = Xerox8044_Services9;
	else if( strcmp(deviceString, "Xerox8044_Services10") == SAME )
		IPDeviceType = Xerox8044_Services10;
	else if( strcmp(deviceString, "Xerox9700_V10") == SAME )
		IPDeviceType = Xerox9700_V10;
	else
		fprintf(stderr, "IP Device unknown: %s\n", deviceName);
}
