/*-
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <varargs.h>
#include <sys/file.h>
#include <sys/stat.h>

static char *program_name;

static char *
stripdir(s)
	register char *s;
{
	register char *cp;
	char *rindex();

	cp = rindex(s, '/');
	return (cp != 0) ? cp + 1 : s;
}

setprog(cp)
	char *cp;
{
	program_name = stripdir(cp);
}

void
usage()
{
	fprintf(stderr, "usage: %s [ -c core ] executable pid\n",
		program_name);
	exit(1);
}

/* VARARGS */
void
error(va_alist)
	va_dcl
{
	register char *cp;
	va_list ap;

	(void)fprintf(stderr, "%s: ", stripdir(program_name));

	va_start(ap);
	cp = va_arg(ap, char *);
	(void)vfprintf(stderr, cp, ap);
	va_end(ap);
	if (*cp) {
		cp += strlen(cp);
		if (cp[-1] != '\n')
			(void)fputc('\n', stderr);
	}
	exit(1);
	/* NOTREACHED */
}

/* VARARGS */
void
warning(va_alist)
	va_dcl
{
	register char *cp;
	va_list ap;

	(void)fprintf(stderr, "%s: warning: ", stripdir(program_name));

	va_start(ap);
	cp = va_arg(ap, char *);
	(void)vfprintf(stderr, cp, ap);
	va_end(ap);
	if (*cp) {
		cp += strlen(cp);
		if (cp[-1] != '\n')
			(void)fputc('\n', stderr);
	}
}
