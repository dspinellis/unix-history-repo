#ifndef lint
static char sccsid[] = "@(#)gets.c	5.1 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>

char *
gets(s)
char *s;
{
	register c;
	register char *cs;

	cs = s;
	while ((c = getchar()) != '\n' && c != EOF)
		*cs++ = c;
	if (c == EOF && cs==s)
		return(NULL);
	*cs++ = '\0';
	return(s);
}
