#ifndef lint
static	char *sccsid = "@(#)string.c	3.1 83/11/22";
#endif

#include "string.h"

char *malloc();

char *
str_cpy(s)
register char *s;
{
	char *str;
	register char *p;

	str = p = malloc((unsigned) strlen(s) + 1);
	if (p == 0)
		return 0;
	while (*p++ = *s++)
		;
	return str;
}

char *
str_itoa(i)
int i;
{
	char buf[30];

	(void) sprintf(buf, "%d", i);
	return str_cpy(buf);
}

char *
str_cat(s1, s2)
char *s1, *s2;
{
	char *str;
	register char *p, *q;

	str = p = malloc((unsigned) strlen(s1) + strlen(s2) + 1);
	if (p == 0)
		return 0;
	for (q = s1; *p++ = *q++;)
		;
	for (q = s2, p--; *p++ = *q++;)
		;
	return str;
}

str_free(str)
char *str;
{
	extern char end[];

	if (str >= end)
		free(str);
}

/*
 * match s against p.
 * s can be a prefix of p with at least min characters.
 */
str_match(s, p, min)
register char *s, *p;
register min;
{
	for (; *s && *p && *s == *p; s++, p++, min--)
		;
	return *s == *p || *s == 0 && min <= 0;
}
