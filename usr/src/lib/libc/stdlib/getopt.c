/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getopt.c	4.4 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <stdio.h>

/*
 * get option letter from argument vector
 */
int	opterr = 1,		/* if error message should be printed */
	optind = 1,		/* index into parent argv vector */
	optopt;			/* character checked for validity */
char	*optarg;		/* argument associated with option */

#define BADCH	(int)'?'
#define EMSG	""
#define tell(s)	{ \
	if (opterr) { \
		fputs(*nargv, stderr); \
		fputs(s, stderr); \
		fputc(optopt, stderr); \
		fputc((int)'\n', stderr); \
	} \
	return(BADCH); \
}

getopt(nargc, nargv, ostr)
	int	nargc;
	char	**nargv, *ostr;
{
	static char	*place = EMSG;		/* option letter processing */
	register char	*oli;			/* option letter list index */
	char	*index();

	if (!*place) {				/* update scanning pointer */
		if (optind >= nargc || *(place = nargv[optind]) != '-' ||
		    !*++place)
			return(EOF);
		if (*place == '-') {		/* found "--" */
			++optind;
			return(EOF);
		}
	}					/* option letter okay? */
	if ((optopt = (int)*place++) == (int)':' ||
	    !(oli = index(ostr, optopt))) {
		if (!*place)
			++optind;
		tell(": illegal option -- ");
	}
	if (*++oli != ':') {			/* don't need argument */
		optarg = NULL;
		if (!*place)
			++optind;
	}
	else {					/* need an argument */
		if (*place)			/* no white space */
			optarg = place;
		else if (nargc <= ++optind) {	/* no arg */
			place = EMSG;
			tell(": option requires an argument -- ");
		}
	 	else				/* white space */
			optarg = nargv[optind];
		place = EMSG;
		++optind;
	}
	return(optopt);				/* dump back option letter */
}
