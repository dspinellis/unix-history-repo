#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gets.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

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
