#ifndef lint
static char sccsid[] = "@(#)puts.c	5.1 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>

puts(s)
register char *s;
{
	register c;

	while (c = *s++)
		putchar(c);
	return(putchar('\n'));
}
