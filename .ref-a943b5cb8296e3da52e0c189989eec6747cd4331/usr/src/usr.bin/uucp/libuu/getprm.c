#ifndef lint
static char sccsid[] = "@(#)getprm.c	5.4 (Berkeley) %G%";
#endif

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
