/* @(#)gets.c	4.2 (Berkeley) %G% */
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
