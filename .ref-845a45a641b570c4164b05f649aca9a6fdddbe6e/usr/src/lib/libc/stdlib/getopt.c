/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getopt.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * get option letter from argument vector
 */
int	opterr = 1,		/* if error message should be printed */
	optind = 1,		/* index into parent argv vector */
	optopt,			/* character checked for validity */
	optreset;		/* reset getopt */
char	*optarg;		/* argument associated with option */

#define	BADCH	(int)'?'
#define	BADARG	(int)':'
#define	EMSG	""

int
getopt(nargc, nargv, ostr)
	int nargc;
	char * const *nargv;
	const char *ostr;
{
	static char *place = EMSG;		/* option letter processing */
	register char *oli;			/* option letter list index */
	char *p;

	if (optreset || !*place) {		/* update scanning pointer */
		optreset = 0;
		if (optind >= nargc || *(place = nargv[optind]) != '-') {
			place = EMSG;
			return(EOF);
		}
		if (place[1] && *++place == '-') {	/* found "--" */
			++optind;
			place = EMSG;
			return(EOF);
		}
	}					/* option letter okay? */
	if ((optopt = (int)*place++) == (int)':' ||
	    !(oli = index(ostr, optopt))) {
		/*
		 * if the user didn't specify '-' as an option,
		 * assume it means EOF.
		 */
		if (optopt == (int)'-')
			return(EOF);
		if (!*place)
			++optind;
		if (opterr && *ostr != ':') {
			if (!(p = rindex(*nargv, '/')))
				p = *nargv;
			else
				++p;
			(void)fprintf(stderr, "%s: illegal option -- %c\n",
			    p, optopt);
		}
		return(BADCH);
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
			if (!(p = rindex(*nargv, '/')))
				p = *nargv;
			else
				++p;
			if (*ostr == ':')
				return(BADARG);
			if (opterr)
				(void)fprintf(stderr,
				    "%s: option requires an argument -- %c\n",
				    p, optopt);
			return(BADCH);
		}
	 	else				/* white space */
			optarg = nargv[optind];
		place = EMSG;
		++optind;
	}
	return(optopt);				/* dump back option letter */
}
