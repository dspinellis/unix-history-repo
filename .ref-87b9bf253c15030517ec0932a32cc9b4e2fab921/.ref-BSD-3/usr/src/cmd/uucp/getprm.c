#include <stdio.h>

#define LQUOTE	'('
#define RQUOTE ')'
#define NOSYSPART	0
#define HASSYSPART	1

/*******
 *	char *
 *	getprm(s, prm)	get next parameter from s
 *	char *s, *prm;
 *
 *	return - pointer to next character in s
 */

char *
getprm(s, prm)
char *s, *prm;
{
	char *c;

	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;

	*prm = '\0';
	if (*s == '\0')
		return(NULL);

	if (*s == '>' || *s == '<' || *s == '|'
	|| *s == ';') {
		*prm++ = *s++;
		*prm = '\0';
		return(s);
	}

	/* look for quoted argument */
	if (*s == LQUOTE) {
		if ((c = index(s + 1, RQUOTE)) != NULL) {
			c++;
			while (c != s)
				*prm++ = *s++;
			*prm = '\0';
			return(s);
		}
	}

	while (*s != ' ' && *s != '\t' && *s != '<'
	&& *s != '>' && *s != '|' && *s != '\0'
	&& *s != ';' && *s != '\n')
		*prm++ = *s++;
	*prm = '\0';

	return(s);
}

/***
 *	split(name, sys, rest)	split into system and file part
 *	char *name, *sys, *rest;

 *
 *	return codes:
 *		NOSYSPART
 *		HASSYSPART
 */

split(name, sys, rest)
char *name, *sys, *rest;
{
	char *c;
	int i;

	if (*name == LQUOTE) {
		if ((c = index(name + 1, RQUOTE)) != NULL) {
		/* strip off quotes */
			name++;
			while (c != name)
				*rest++ = *name++;
			*rest = '\0';
			*sys = '\0';
			return(NOSYSPART);
		}
	}

	if ((c = index(name, '!')) == NULL) {
		strcpy(rest, name);
		*sys = '\0';
		return(NOSYSPART);
	}

	*c = '\0';
	for (i = 0; i < 7; i++)
		if ((*sys++ = *name++) == '\0')
			break;

	strcpy(rest, ++c);
	return(HASSYSPART);
}
