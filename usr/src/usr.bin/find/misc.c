/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include "find.h"
 
/*
 * find_getpaths --
 *	remove the path strings from the command line and returns them in
 *	another array.  The find syntax assumes all command arguments up
 *	to the first one beginning with a '-', '(' or '!' are pathnames.
 */
char **
find_getpaths(argvp)
	char ***argvp;
{
	register char **argv;
	char **start;

	/*
	 * find first '-', '(' or '!' to delimit paths; if no paths, it's
	 * an error.  Shift the array back one at the same time, creating
	 * a separate array of pathnames.
	 */
	for (argv = *argvp + 1;; ++argv) {
		argv[-1] = argv[0];
		if (!*argv || **argv == '-' || **argv == '!' || **argv == '(')
			break;
	}

	if (argv == *argvp + 1)
		usage();

	argv[-1] = NULL;

	start = *argvp;			/* save beginning of path array */
	*argvp = argv;			/* move argv value */
	return(start);			/* return path array */
}
    
/*
 * find_subst --
 *	Replace occurrences of {} in s1 with s2 and return the result string.
 *	Find_subst always returns a newly allocated string which should be
 *	freed by the caller.
 */
find_subst(orig, store, path, len)
	char *orig, **store, *path;
	int len;
{
	register int plen;
	register char ch, *p;
	char *realloc();

	plen = strlen(path);
	for (p = *store; ch = *orig; ++orig)
		if (ch == '{' && orig[1] == '}') {
			while ((p - *store) + plen > len)
				if (!(*store = realloc(*store, len *= 2))) {
					(void)fprintf(stderr,
					    "find: %s.\n", strerror(errno));
					exit(1);
				}
			bcopy(path, p, plen);
			p += plen;
			++orig;
		} else
			*p++ = ch;
	*p = '\0';
}

/*
 * find_queryuser --
 *	print a message to standard error and then read input from standard
 *	input. If the input is 'y' then 1 is returned.
 */
find_queryuser(argv)
	register char **argv;
{
	char buf[10];

	(void)fprintf(stderr, "\"%s", *argv);
	while (*++argv)
		(void)fprintf(stderr, " %s", *argv);
	(void)fprintf(stderr, "\"? ");
	(void)fflush(stderr);
	return(!fgets(buf, sizeof(buf), stdin) || buf[0] != 'y' ? 0 : 1);
}
 
/*
 * bad_arg --
 *	print out a bad argument message.
 */
void
bad_arg(option, error)
	char *option, *error;
{
	(void)fprintf(stderr, "find: %s: %s.\n", option, error);
	exit(1);
}
 
/*
 * emalloc --
 *	malloc with error checking.
 */
char *
emalloc(len)
	u_int len;
{
	char *p, *malloc();

	if (!(p = malloc(len))) {
		(void)fprintf(stderr, "find: %s.\n", strerror(errno));
		exit(1);
	}
	return(p);
}

usage()
{
	(void)fprintf(stderr, "usage: find path-list expression\n");
	exit(1);
}
