/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)getprm.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "uucp.h"

#define LQUOTE	'('
#define RQUOTE ')'

/*LINTLIBRARY*/

/*
 *	get next parameter from s
 *
 *	return - pointer to next character in s
 */

char *
getprm(s, prm)
register char *s, *prm;
{
	register char *c;

	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;

	*prm = '\0';
	if (*s == '\0')
		return NULL;

	if (*s == '>' || *s == '<' || *s == '|'
	  || *s == ';' || *s == '&') {
		*prm++ = *s++;
		*prm = '\0';
		return s;
	}

	/* look for quoted argument */
	if (*s == LQUOTE) {
		if ((c = index(s + 1, RQUOTE)) != NULL) {
			c++;
			while (c != s)
				*prm++ = *s++;
			*prm = '\0';
			return s;
		}
	}

	/* look for `  ` string */
	if (*s == '`') {
		if ((c = index(s + 1, '`')) != NULL) {
			c++;
			while (c != s)
				*prm++ = *s++;
			*prm = '\0';
			return s;
		}
	}

	while (*s != ' ' && *s != '\t' && *s != '<'
		&& *s != '>' && *s != '|' && *s != '\0'
		&& *s != '&' && *s != ';' && *s != '\n')
		*prm++ = *s++;
	*prm = '\0';

	return s;
}
