#ifndef lint
static char sccsid[] = "@(#)getprm.c	5.3 (Berkeley) %G%";
#endif

#include "uucp.h"

#define LQUOTE	'('
#define RQUOTE ')'
#define NOSYSPART	0
#define HASSYSPART	1

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

/*
 *	split into system and file part
 *
 *	return codes:
 *		NOSYSPART
 *		HASSYSPART
 */

split(name, sys, rest)
register char *name, *rest;
char *sys;
{
	register char *c;
	register int i;

	if (*name == LQUOTE) {
		if ((c = index(name + 1, RQUOTE)) != NULL) {
		/* strip off quotes */
			name++;
			while (c != name)
				*rest++ = *name++;
			*rest = '\0';
			*sys = '\0';
			return NOSYSPART;
		}
	}

	if ((c = index(name, '!')) == NULL) {
		strcpy(rest, name);
		*sys = '\0';
		return NOSYSPART;
	}

	*c++ = '\0';
	strncpy(sys, name, MAXBASENAME);
	sys[MAXBASENAME] = '\0';

	strcpy(rest, c);
	return HASSYSPART;
}
