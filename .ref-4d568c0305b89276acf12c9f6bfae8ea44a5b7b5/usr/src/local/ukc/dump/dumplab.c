#ifndef lint
static char *sccsid = "@(#)dumplab.c	1.3 (UKC) %G%";
#endif not lint
/*
 *	This file included by Peter Collinson
 *	to handle tape labelling 
 *	There are two dump parameters which are used to specify labels
 *	-l	Give the basic label format string - this may contain 
 *		a single %s to insert the volume number string
 *	-m	Map string - used to map volume numbers into a string
 *
 *	Routines are:
 *	storelabel(arg)		- called from main() to store a label format
 *
 *	storelabelmap(arg)	- called from main() to process a map argument
 *				- which is
 *					string		simple string
 *					string-string	expanded by incrementing
 *					string,string,..list of the above
 *	char *
 *	createlabel(volno)	- returns a label appropriate for the volume
 *					
 *	initialtape()		- called to print an operator message asking for
 *				- the 1st tape 
 *	
 *	labelest(etapes)	- checks if there are enough tape labels
 *				- specified for a given dump
 *
 *	labelcheck()		- reads one record from tape
 *				- checks that the labels match
 *				- backspace one record back
 *				- so that multi-volume dumps work
 *
 *	log_volume()		- write a logfile entry for the volume
 *	
 */

#include "dump.h"
#include <math.h>


#define	LABMAX	100		/* get space for 100 */

char	*labfmt;		/* Basic tape format */

char	*labarg[LABMAX];	/* Pointer to argument list */

int	labct;			/* number of entries */
				/* if zero then no labels used */

int	userlabel;		/* set if user has given a label */

int	labchk;			/* check labels - set by t(est) flag */

/*
 *	The file /etc/dumpvolumes is used to maintain a log of
 *	tapes which are/have been used for tape dumping
 *	The format is:
 *	label: date dev=<devname> level=<dump level> reel=<volume number> inode=<start inode>
 */
char dumpvolumes[] = "/etc/dumpvolumes";

#ifdef RDUMP
extern char *host;
#endif RDUMP

/*
 *	Called from argument decoding to store the
 *	basic label format
 *	This is the parameter to the -l parameter
 */
storelabel(arg)
	char *arg;
{
	labfmt = arg;
	userlabel = 1;
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
	
	userlabel = 1;

	/*
	 *	Parse the argument looking for a single string
	 */
	for (ss = arg; *ss; ss = es) {
		es = labskip(ss);
		lastc = *es;	/* save last character */
		*es++ = '\0';	/* make the first label into a string */
		if (labct > LABMAX)
			labfatal("Too many (> %d) tape labels specified\n", LABMAX);
		if (*ss == '\0')
			labfatal("Zero length tape label found\n");
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
			if (lastc)
				*es++ = '\0';
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
		carry = 0;
		do {
			if (isdigit(*incr)) {
				if (*incr == '9') {
					*incr = '0';
					carry = 1;
				} else
					(*incr)++;
			} else
			if (isupper(*incr)) {
				if (*incr == 'Z') {
					*incr = 'A';
					carry = 1;
				} else
					(*incr)++;
			} else
			if (islower(*incr)) {
				if (*incr == 'z') {
					*incr = 'a';
					carry = 1;
				} else
					(*incr)++;
			} else
				labfatal("Problem with label map range spec - can only increment alphanumeric values\n");
			if (carry) {
				incr--;
				if (incr < startrange)
					labfatal("Problem with label map range spec - end of range reached\n");
			}
		} while (carry);
		
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

initialtape()
{	static firstpr;

	if (labchk == 0)
		return;
	if (firstpr == 0)
		msg("Mount tape %s for reel 1 of the dump\n", createlabel(1));
	firstpr = 1;
}

/*
 *	given an estimated number of tapes, check that
 *	there are enough tapes on the label list
 */
labelest(etapes)
	double etapes;
{	int et;

	if (labct) {
		et = ceil(etapes);
		if (et > labct)
			labfatal("Only %d labe%s given, estimated need %d\n",
				labct, labct == 1 ? "l" : "ls", et);
	}
}

/*
 *	labelcheck
 *	read a dump header and check that the tape header contains
 *	the label we expected
 *	close the tape after use
 */
labelcheck(tno)
	int tno;
{	
	register fd;
	union u_spcl uin;
	register char *label;
	register char *ontape = uin.s_spcl.c_label;
	
	if (labchk == 0 || pipeout)
		return(0);
	label = createlabel(tno);

#ifdef RDUMP
	/* doing it this way to make it easier to read */
	if (host) {
		while (rmtopen(tape, 0) < 0)
			if (!query("Cannot open tape. Do you want to retry the open?"))
				dumpabort();
		if (rmtread((char *)&uin, sizeof uin) != sizeof uin) {
			msg("Tape does not start with the correctly sized record\n");
			(void)rmtclose();
			return(-1);
		}
		if (ontape[0] == '\0' ||
		    strcmp(ontape, "none") == 0 ||
		    strcmp(ontape, label) == 0) {
			(void)rmtclose();
			return(0);
		}
		msg("Tape labels do not match should be `%s' is `%s'\n", label, ontape);
		(void)rmtclose();
		return(-1);
	}
#endif RDUMP
	while ((fd = open(tape, 0)) < 0)
		if (!query("Cannot open tape. Do you want to retry the open?"))
			dumpabort();
	if (read(fd, (char *)&uin, sizeof uin) != sizeof uin) {
		msg("Tape does not start with the correctly sized record\n");
		(void)close(fd);
		return(-1);
	}
	if (ontape[0] == '\0' ||
	    strcmp(ontape, "none") == 0 ||
	    strcmp(ontape, label) == 0) {
		(void)close(fd);
		return(0);
	}
	msg("Tape labels do not match should be `%s' is `%s'\n", label, ontape);
	(void)close(fd);
	return(-1);
}

/*
 *	write a log entry for the volume into the log file
 */
log_volume(tlabel)
	char *tlabel;
{
	char *ctime();
	FILE *logfd;

	if (uflag == 0 || labchk == 0)
		return;
	if ((logfd = fopen(dumpvolumes, "a")) == NULL)
		return;
	fprintf(logfd, "%s: date=%20.20s dev=%s level=%c reel=%d ino=%d\n",
			tlabel, ctime(&spcl.c_date)+4, disk, incno, tapeno,
			tapeno == 1 ? ROOTINO : ino);
	fclose(logfd);
}

/*
 *	skip forward looking for valid end of label characters
 */
/* static */
char *
labskip(str)
	register char *str;
{	
	while (*str != ',' && *str != '-' && *str)
		str++;
	return (str);
}

/*
 *	generate a fatal error message
 */
	/* VARARGS1 */
	/* ARGSUSED */
labfatal(fmt, a1, a2, a3, a4, a5)
	char	*fmt;
	int	a1, a2, a3, a4, a5;
{	
	msg(fmt, a1, a2, a3, a4, a5);
	dumpabort();
}

