/* @(#)puts.c	4.1 (Berkeley) 12/21/80 */
#include	<stdio.h>

puts(s)
register char *s;
{
	register c;

	while (c = *s++)
		putchar(c);
	return(putchar('\n'));
}
