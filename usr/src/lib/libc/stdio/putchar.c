#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)putchar.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * A subroutine version of the macro putchar
 */
#include <stdio.h>

#undef putchar

putchar(c)
register c;
{
	putc(c, stdout);
}
