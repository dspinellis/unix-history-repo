#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)puts.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include	<stdio.h>

puts(s)
register char *s;
{
	register c;

	while (c = *s++)
		putchar(c);
	return(putchar('\n'));
}
