#include	<stdio.h>

puts(s)
register char *s;
{
	register c;

	while (c = *s++)
		putchar(c);
	return(putchar('\n'));
}
