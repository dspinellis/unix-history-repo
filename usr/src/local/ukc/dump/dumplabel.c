#ifndef lint
static char *sccsid = "@(#)dumplabel.c	1.1 (UKC) %G%";
#endif not lint
/***

* program name:
	dumplabel.c
* function:
	prints the label from a header
	writes a dummy dump header at the start of a tape
	Doesn't do any checksum
* switches:
	-f <tapename>
	-r	rewind/offline each tape
	-l	label format
	followed by list of tape labels in the same format at given to the
		locally modified dump program
	if no tape labels are given print the details on the start of a tape
* libraries used:
	standard
* compile time parameters:
	cc -o dumplabel dumplabel.c
* history:
	Written August 1987 Peter Collinson UKC

***/
#include "dump.h"
#include <sys/ioctl.h>
#include <sys/mtio.h>

#define	LABMAX	100		/* get space for 100 */

char	*labarg[LABMAX];	/* vector of map entries */

char	*labfmt;		/* format for tape label */

int	labct;			/* number of entries */
				/* if zero then no labels used */

char *tape = "/dev/rmt8";

int	dorew;			/* set if rewind offline on exit */
int	tapecount;		/* number of tapes dealt with */


main(argc, argv)
char **argv;
{	register i;
	register char *p;
	while (--argc) {
		argv++;
		if (**argv == '-') {
			for (p = *argv; *p; p++)
				switch (*p) {

				case 'f':
					if (--argc > 0) {
						argv++;
						tape = *argv;
					}
					break;
				case 'r':
					dorew++;	/* no rewind/offline */
					break;
				case 'l':
					if (--argc > 0) {
						argv++;
						labfmt = *argv;
					}
				}
		} else {
			storelabelmap(arg);
			for (i = 1; i <= labct; i++)
				dolabel(i);
			labct = 0;
		}
	}
	if (tapecount == 0) {
		if (labfmt) {
			labct = 0;
			dolabel(1);	/* pretend a volume number of 1 */
		} else
			printlabel()
	}
	exit(0);
}

/*
 *	Store map list
 *	The map list 
 *	allows a simple way to specify a range of tapes
 *	This generates a string which is inserted into the label format
 *	by use of an sprint operation
 *
 *	The basic form here is:
 *	<string>			a single string
 *	<string>,<string>,......	a list of strings
 *	<string>-<string>		a range of strings
 *					where the string is `incremented'
 *					to generate a list
 */
storelabelmap(arg)
	char *arg;
{
	register char *ss, *es;
	register char *incbase, *incr;
	register lastc;
	char *labskip();
	char *strstore();

	/*
	 *	Parse the argument looking for a single string
	 */
	for (ss = arg; *ss; ss = es, labct++) {
		es = labskip(ss);
		lastc = *es;	/* save last character */
		*es++ = '\0';	/* make the first label into a string */
		if (labct > LABMAX)
			labfatal("Too many (> %d) tape labels specified\n", LABMAX);
		labarg[labct++] = strstore(ss);

		if (lastc == 0)
			break;		/* end of list */

		if (lastc == '-') {
			/*
			 * this gives a tape range
			 * increment the source number until it equals the final
			 * value
			 */
			incbase = ss;
			ss = es;
			es = labskip(ss);
			if (*es == '-')
				labfatal("Range has the format <string>-<string>\n");
			lastc = *es;
			*es = '\0';
			/*
			 * basic test the source string length must be equal to the
			 * end string length
			 */
			if (strlen(incbase) != strlen(ss))
				labfatal("strlen(\"%s\") != strlen(\"%s\")\n", incbase, ss);
			labelrange(incbase, ss);
		}
	}
}

/*
 *	Expand a label range
 */
/* static */
labelrange(startrange, endrange)
	char *startrange, *endrange;
{
	register char *incr;
	register int carry;
	
	
	for (incr = startrange + strlen(startrange) - 1;
			strcmp(startrange, endrange) != 0; ) {
		/* start incrementing */
		for (carry = 0; carry; ) {
			if (isdigit(*incr)) {
				if (*incr == '9') {
					*incr = '0';
					carry = 1;
				} else
					*incr++;
			} else
			if (isupper(*incr)) {
				if (*incr == 'Z') {
					*incr = 'A';
					carry = 1;
				} else
					*incr++;
			} else
			if (islower(*incr)) {
				if (*incr == 'z') {
					*incr = 'a';
					carry = 1;
				} else
					*incr++;
			} else
				labfatal("Problem with label map range spec - can only increment alphanumeric values\n");
			if (carry) {
				incr--;
				if (incr < startrange)
					labfatal("Problem with label map range spec - end of range reached\n");
			}
		}
		if (labct > LABMAX)
			labfatal("Too many (> %d) tape labels specified\n", LABMAX);
		labarg[labct++] = strstore(startrange);

	}
}

/*
 *	Store a string using malloc
 */
/* static */
char *
strstore(arg)
	char *arg;
{
	register len = strlen(arg)+1;
	register char *dest;
	char *malloc();

	dest = malloc(len);
	if (dest == NULL)
		labfatal("No memory for string storage\n");
	bcopy(arg, dest, len);
	return(dest);
}

/*
 *	Create a tape label from a volume number
 */
char *
createlabel(volno)
	int volno;
{
	static char buf[LBLSIZE+LBLSIZE];
	static int lastvol;
	char volbuf[8];
	register char *arg;
	
	if (userlabel == 0)
		return ("none");		/* previous behaviour */

	if (volno == lastvol)			/* cache */
		return(buf);
	lastvol = volno;
	
	if (labfmt == NULL)
		labfmt = "%s";

	if (labct == 0)
	{	(void) sprintf(volbuf, "%d", volno);
		arg = volbuf;
	}
	else		
		arg = labarg[volno-1];		/* volumes run 1-> */
	(void) sprintf(buf, labfmt, arg);
	buf[LBLSIZE-1] = '\0';			/* Truncate to correct size */
	return(buf);
}

/*
 *	skip forward looking for valid end of label characters
 */
char *
labskip(str)
register char *str;
{	while (*str != ',' && *str != '-' && *str)
		str++;
	return (str);
}

/*
 *	do the actual labelling
 */
dolabel(index)
{	register fd;

	if (tapecount++)
		askformount(index);

	while ((fd = open(tape, 2)) < 0)
	{	fprintf(stderr, "Tape open error\n");
		askformount(index);
	}
	strcpy(spcl.c_label, createlabel(index));
	if (write(fd, (char *)&u_spcl, sizeof (u_spcl)) != sizeof (u_spcl))
	{	perror("Tape write error");
		exit(1);
	}
	rewind(fd);
	close(fd);
}

/*
 *	ask for a tape to be mounted
 */
askformount(index)
{
	while (query("Mount tape `%s'\nReady to go?", createlabel(index)) != 1);
}

/*
 *	ask yes/no question
 *	return	1 on yes
 *		0 on no
 */
query(question, arg)
char	*question;
char *arg;
{
	char	replybuffer[64];
	int	back;

	for(;;)
	{ 
		fprintf(stdout, question, arg);
		fflush(stdout);
		if (fgets(replybuffer, 63, stdin) == NULL)
		{	if(ferror(stdin))
			{	clearerr(stdin);
				continue;
			}
		}
		else
		if ((strcmp(replybuffer, "yes\n") == 0) ||
			    (strcmp(replybuffer, "Yes\n") == 0))
		{
			back = 1;
			goto done;
		}
		else
		if ((strcmp(replybuffer, "no\n") == 0) ||
			    (strcmp(replybuffer, "No\n") == 0))
		{
			if (query("Abort? "))
				labfatal("Aborting\n");
			back = 0;
			goto done;
		}
		else
			fprintf(stderr, "\"Yes\" or \"No\" ONLY!\n");
	}
    done:
	return(back);
}

rewind(fd)
{
	struct mtop mtop;
	if (dorew)
	{	mtop.mt_op = MTOFFL;
		mtop.mt_count = 1;
		ioctl(fd, MTIOCTOP, &mtop);
	}
}

/*
 *	fatal error message routine
 */
labfatal(fmt, a1, a2, a3, a4, a5)
char	*fmt;
int	a1, a2, a3, a4, a5;
{	fprintf(stderr, fmt, a1, a2, a3, a4, a5);
	exit(1);
}

/*
 *	print the label from a tape
 */
printlabel()
{
	register fd;
	
	if ((fd = open(tape, 2)) < 0)
	{	perror("Tape open error");
		exit(1);
	}
	if (read(fd, (char *)&u_spcl, sizeof u_spcl) < 0)
	{	perror("Tape read");
		exit(1);
	}
	printf("%s\n", spcl.c_label);
	close(fd);
}
