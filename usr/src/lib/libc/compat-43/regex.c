/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James da Silva at the University of Maryland at College Park.
 *
 * %sccs.include.redist.c%
 */

/*
 * Compatibility routines that implement the old re_comp/re_exec interface in
 * terms of the regcomp/regexec interface.  It's possible that some programs
 * rely on dark corners of re_comp/re_exec and won't work with this version,
 * but most programs should be fine.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)regex.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stddef.h>
#include <regexp.h>
#include <string.h>
#include <stdlib.h>

static regexp *re_regexp;
static int re_goterr;
static char *re_errstr;

char *
re_comp(s)
	char *s;
{
	if (s == NULL)
		return (NULL);
	if (re_regexp)
		free(re_regexp);
	if (re_errstr)
		free(re_errstr);
	re_goterr = 0;
	re_regexp = regcomp(s);
	return (re_goterr ? re_errstr : NULL);
}

int
re_exec(s)
	char *s;
{
	int rc;

	re_goterr = 0;
	rc = regexec(re_regexp, s);
	return (re_goterr ? -1 : rc);
}

void
regerror(s)
	const char *s;
{
	re_goterr = 1;
	if (re_errstr)
		free(re_errstr);
	re_errstr = strdup(s);
}
