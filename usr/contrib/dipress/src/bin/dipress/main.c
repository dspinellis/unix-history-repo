/******************************************************************************
 *
 *	main	--	main routine for dipress 
 *			(ditroff to interpress conversion)
 *
 *	John Mellor-Crummey (Xerox Corp)
 *
 *	Copyright 1985 Xerox Corporation
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

	/* set up signal handlers to insure clean termination */
	signalHandler();	

	/* process command line options */

	while ((--argc > 0) && (**++argv == '-'))
	{
		switch (*++*argv) 
		{
		case 'd':		
			/* produce debugging info */
			dbg = atoi(++*argv);
			if (dbg == 0) dbg = 1;
			setbuf(stdout, (char *)NULL);
			printf("debugging level %d\n", dbg);
			break;
		case 'f':
		case 'F':
			/* reset default font directory 
			 * accept either form '-f <dir>' or '-f<dir>'
			 */
			if (*(*argv+1) == '\0')
			{
				fontdirectory = *++argv;
				--argc;
			}
			else
				fontdirectory = ++*argv;
			break;
		case 'o':		
			/* read the following list of page numbers */
			readPageList(++*argv);
			break;
		case 't':		
			/* dump the interpress to the standard output */
			outputfile = fileno(stdout);
			break;
		default:
			reportError(CONTINUE,"option -%c not recognized\n", (char *) **argv);
			break;
		}
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



	if (argc <= 0)	/* if no file specified, default to stdin */
		ditroffToIpress(stdin);
	else
		/* process files explicitly specified */
		while (argc-- > 0)
		{
			if ((inputfile = fopen(*argv++, "r")) == NULL)
				reportError(QUIT, "can't open %s: %s",
					*argv, sys_errlist[errno]);
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
	
