static char *rcsid = "$Header$";
/*
 * plog - record, edit, print, sort progress of a project
 *
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include "bin.h"
#include "date.h"
#include "from.h"
#include "getarg.h"
#include "null.h"
#include "path.h"
#include "spms.h"
#include "system.h"
#include "yesno.h"

#define SUBJECT_MAX_FORWARD_LOOK 5	/* max no. of lines to read ahead */
					/* for "Subject: " */
#define TITLELENGTH		72	/* length of subject title */
/*
 * type of operation to be executed by plog
 */
#define APPENDLOG	1
#define EDITLOG		2
#define PRINTLOG	3
#define PRINTLOGTITLE	4
#define SORTLOG		5

char *PGN = "plog";			/* program name */
int PRINT_TITLE = YES;			/* print message titles? */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char command[PATHSIZE+8];	/* shell command buffer */
	char *cwp;			/* current working project */
	char *getcwp();			/* get current working project */
	char *pathcat();		/* pathname concatenation */
	char *pathname;			/* regular pathname */
	char plog[PATHSIZE];		/* PROJECTLOG pathname */
	char *strcpy();			/* string copy */
	FILE *ifp;			/* input file stream */
	FILE *mustfopen();		/* must open file or die */
	FILE *ofp;			/* output file stream */
	FILE *popen();			/* open pipe */
	int action;			/* what plog has to do */
	int decoderange();		/* decode message range */
	int naction = 0;		/* number of non-default actions */
	int range[2];			/* range of messages to print */
	int sortlog();			/* sort project log by date */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	void print();			/* print PROJECTLOG */
	void print_head();		/* print PROJECTLOG message headings */

	action = APPENDLOG;
	range[0] = range[1] = 0;

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && (**++argv == '-' || **argv == '+'))
		{
		if (**argv == '-')
			{
			for (s = argv[0]+1; *s != '\0'; s++)
				switch (*s)
					{
					case 'D':
						PPDEBUG = YES;
						break;
					case 'e':
						action = EDITLOG;
						naction++;
						break;
					case 'h':
						PRINT_TITLE = NO;
						break;
					case 'p':
						action = PRINTLOG;
						naction++;
						if (decoderange(++s, range) == NO)
							status = 1;
						goto endif;
					case 's':
						action = SORTLOG;
						naction++;
						break;
					default:
						warn("bad option -%c", *s);
						status = 1;
						goto endif;
					}
			}
		else	{
			for (s = argv[0]+1; *s != '\0'; s++)
				switch (*s)
					{
					case 'h':
						action = PRINTLOGTITLE;
						naction++;
						break;
					default:
						warn("bad option +%c", *s);
						status = 1;
						goto endif;
					}
			}
		endif: continue;
		}
	}
	if (status == 1 || argc > 1)
		{
		fatal("usage: plog [-e] [{+-}h] [-p[low[-high]]] [-s] [projectname]");
		}
	if (naction > 1)
		{
		fatal("choose only one of -e, -p, -s, or +h options");
		}
	if (argc == 0)
		{
		if ((cwp = getcwp()) == NULL)
			fatal("no project environment");
		pathname = cwp;
		}
	else	{
		if (xppath(*argv, &pathbuf) == -1)
			{
			patherr(*argv);
			exit(1);
			}
		switch (pathbuf.p_mode & P_IFMT)
			{
			case P_IFNEW:
			case P_IFREG:
			case P_IFPDIR:
				fatal("%s: no such project", *argv);
			case P_IFHOME:
			case P_IFPROOT:
				pathname = pathbuf.p_path;
				break;
			}
		}
	pathcat(plog, pathname, PROJECTLOG);
	
	switch (action)
		{
		case APPENDLOG:
			printf("Mailing to %s\n", plog);
			sprintf(command, "Mail %s", plog);
			status = system(command);
			break;
		case EDITLOG:
			printf("Editing %s\n", plog);
			sprintf(command, "Mail -f %s", plog);
			status = system(command);
			break;
		case PRINTLOG:
			ifp = mustfopen(plog, "r");
			if (!isatty(fileno(stdout)) || (ofp = popen("more","w")) == NULL)
				ofp = stdout;

			print(ifp, ofp, range);

			if (ofp != stdout)
				pclose(ofp);
			break;
		case PRINTLOGTITLE:
			ifp = mustfopen(plog, "r");
			if (!isatty(fileno(stdout)) || (ofp = popen("more","w")) == NULL)
				ofp = stdout;

			print_head(ifp, ofp);

			if (ofp != stdout)
				pclose(ofp);
			break;
		case SORTLOG:
			printf("Sorting %s\n", plog);
			status = sortlog(plog);
			break;
		}
	exit(status);
}



/*
 * decoderange() determines the range of message numbers to be printed.
 * Prints an error message and returns NO if syntax error, otherwise YES.
 */
decoderange(srange, range)
	char *srange;			/* range string to decode */
	int range[];			/* decoded message range */
{
	register char *s;		/* range string pointer */
	register int high = 0;		/* high end of message range */
	register int low = 0;		/* low end of message range */

	for (s = srange; isdigit(*s); s++)
			low = 10*low + (*s - '0');
	if (*s == '-')
		for (s++; isdigit(*s); s++)
				high = 10*high + (*s - '0');
	if (*s != '\0')
		{
		warn("%s: bad message range", srange);
		return(NO);
		}
	range[0] = low;
	range[1] = high;
	return(YES);
}



/*
 * print() copies input stream to output stream, printing a title at
 * the beginning of each log entry.
 */
void
print(ifp, ofp, range)
	register FILE *ifp;		/* input file stream */
	register FILE *ofp;		/* output file stream */
	int range[];			/* range of messages to print */
{
	register int high;		/* top of range */
	register int low;		/* bottom of range */
	register int msgno;		/* current message number */
	char *fgets();			/* get a line from input stream */
	char linebuf[BUFSIZ];		/* input line buffer */
	FROM *isfrom();			/* is line a "From " line? */
	void print_title();		/* print message title */

	msgno = 0;
	low = range[0];
	high = range[1];

	while (fgets(linebuf, BUFSIZ, ifp) != NULL)
		if (isfrom(linebuf) != NULL)
			{
			msgno++;
			if (msgno < low)
				continue;
			else if (msgno > high && high != 0)
				break;
			if (PRINT_TITLE == YES)
				{
				print_title(linebuf, ifp, ofp);
				}
			else	{
				fputs(linebuf, ofp);
				}
			}
		else	{
			if (msgno < low)
				continue;
			fputs(linebuf, ofp);
			}
}



/*
 * print_head() prints the log entry headings only.
 */
void
print_head(ifp, ofp)
	register FILE *ifp;		/* input file stream */
	register FILE *ofp;		/* output file stream */
{
	register int msgno;		/* current message number */
	register char *sp;		/* subject field pointer */
	char *fgets();			/* get a line from input stream */
	char linebuf[BUFSIZ];		/* input line buffer */
	char *skipword();		/* skip to next word */
	char *subject;			/* beginning of subject field */
	FROM *fromline;			/* "From " line struct */
	FROM *isfrom();			/* is line a "From " line? */
	int i;				/* read-ahead buffer counter */
	int strlen();			/* string length */
	int strncmp();			/* compare strings for n chars */

	msgno = 0;
	while (fgets(linebuf, BUFSIZ, ifp) != NULL)
		if ((fromline = isfrom(linebuf)) != NULL)
			{
			msgno++;
			fprintf(ofp, "%3d %-10s %s", msgno, fromline->from, 
				fromline->date);
			for (i = 0; i < SUBJECT_MAX_FORWARD_LOOK; i++)
				{
				if (fgets(linebuf, BUFSIZ, ifp) == NULL)
					break;
				if (strncmp("Subject: ", linebuf, 9) == 0)
					{
					sp = subject = skipword(linebuf);
					while (*sp != '\n' && *sp != '\0')
						sp++;
					*sp = '\0';
					fprintf(ofp, " \"%s\"", subject);
					}
				else if (isfrom(linebuf) != NULL)
					{
					/* "From " line not welcome here */
					fseek(ifp, (long) -strlen(linebuf), 1);
					break;
					}
				}
			putc('\n', ofp);
			}
}



/*
 * printsubject() pretty-prints a "Subject: " field.
 */
void
printsubject(linebuf, ofp)
	char *linebuf;			/* line containing subject */
	register FILE *ofp;		/* output file stream */
{
	register int nblank;		/* number of blanks to pad title */
	char *skipword();		/* skip to next word */
	int strlen();			/* string length */

	nblank = (TITLELENGTH - (strlen(linebuf) - 9)) / 2;
					/* length of "Subject: " = 9 chars */
	while (nblank-- > 0)
		putc(' ', ofp);
	fputs(skipword(linebuf), ofp);
}



/*
 * print_title() prints a subject title between two dashed lines. 
 * If a "Subject:" field cannot be found within SUBJECT_MAX_FORWARD_LOOK
 * lines of the "From" line, then, a single dash line is printed.
 */
#define PUTDASHLINE(fp)	{int i = TITLELENGTH; \
			while (i-- > 0) putc('-', fp); putc('\n', fp);}
void
print_title(linebuf, ifp, ofp)
	char linebuf[];			/* line buffer containing "From" */
	register FILE *ifp;		/* input file stream */
	register FILE *ofp;		/* output file stream */
{
	char *fgets();			/* get a line from input stream */
	char frombuf[BUFSIZ];		/* "From " line buffer */
	char *strcpy();			/* string copy */
	int i;				/* read-ahead buffer counter */
	int strncmp();			/* compare strings for n chars */
	long ftell();			/* offset relative to file beginning */
	long markifp;			/* mark position of file */
	void printsubject();		/* printprint "Subject: " field */

	PUTDASHLINE(ofp);

	markifp = ftell(ifp);
	strcpy(frombuf, linebuf);
	for (i = 0; i < SUBJECT_MAX_FORWARD_LOOK; i++)
		{
		if (fgets(linebuf, BUFSIZ, ifp) == NULL)
			break;
		if (strncmp("Subject: ", linebuf, 9) == 0)
			{
			printsubject(linebuf, ofp);
			PUTDASHLINE(ofp);
			break;
			}
		}
	fputs(frombuf, ofp);
	fseek(ifp, markifp, 0);
}



/*
 * sortlog() sorts the project log by date. Returns status 0 if
 * successful, otherwise 1.
 */
sortlog(logname)
	char *logname;			/* name of project log file */
{
	register FILE *ifp;		/* input file stream */
	register int (*hstat)();	/* hangup status */
	register int (*istat)();	/* interrupt status */
	register int (*qstat)();	/* quit status */
	register long nc = 0;		/* number of characters read */
	char *fgets();			/* get a line from input stream */
	char linebuf[BUFSIZ];		/* input line buffer */
	char *pathcat();		/* pathname concatenation */
	char *pathhead();		/* remove pathname tail */
	char *strcpy();			/* string copy */
	FILE *mustfopen();		/* must open file or die */
	FILE *ofp;			/* output file stream */
	FROM *from;			/* broken down "From " line */
	FROM *initfrom();		/* initialize "From " pointer array */
	FROM *isfrom();			/* is line a "From " line? */
	FROM *lastfrom;			/* previous "From " line */
	FROM *savefrom();		/* save "From " lines */
	int len;			/* length of input line */
	int outfrom();			/* output "From " messages */
	int parsedate();		/* parse ctime(3) generated date */
	char sortlog[PATHSIZE];		/* temporary projectlog for sorting */
	int status = 0;			/* return status */
	int strlen();			/* string length */
	void sortfrom();		/* sort "From " lines */

	ifp = mustfopen(logname, "r");

	lastfrom = initfrom();

	for (;;)
		{
		if (fgets(linebuf, BUFSIZ, ifp) == NULL)
			break;
		len = strlen(linebuf);
		nc += len;
		if ((from = isfrom(linebuf)) != NULL)
			{
			if (parsedate(from->date, &from->bdt) == NO)
				{
				warn("%s: bad date", from->date);
				status = 1;
				}
			else	{
				from->m_seek = nc - len;
				lastfrom->m_len = from->m_seek - lastfrom->m_seek;
				if ((lastfrom = savefrom(from)) == NULL)
					{
					warn("out of memory");
					return(1);
					}
				}
			}
		}
	lastfrom->m_len = nc - lastfrom->m_seek;
	if (status > 0)
		return(status);
	sortfrom();
	pathcat(sortlog, pathhead(strcpy(sortlog, logname)), "temp_log");
	if (FILEXIST(sortlog))
		{
		warn("%s sort in progress - try later", PROJECTLOG);
		return(1);
		}

	hstat = signal(SIGHUP, SIG_IGN);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	ofp = mustfopen(sortlog, "w");
	if (outfrom(ifp, ofp) == YES)
		{
		fclose(ifp);
		fclose(ofp);
		RENAME(sortlog, logname);
		}
	else	{
		warn("write error in %s: sort failed", sortlog);
		fclose(ifp);
		fclose(ofp);
		unlink(sortlog);
		status = 1;
		}

	signal(SIGINT, hstat);
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);

	return(status);
}



/*
 * skipword() skips a liberal (blank, tab delimited) word and returns a
 * pointer to the next word.
 */
char *
skipword(bp)
	register char *bp;		/* buffer pointer */
{
	for (; *bp != '\0' && isspace(*bp); bp++)
		continue;
	for (; *bp != '\0' && !isspace(*bp); bp++)
		continue;
	for (; *bp != '\0' && isspace(*bp); bp++)
		continue;
	return(bp);
}
