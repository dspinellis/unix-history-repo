#ifndef lint
static char sccsid[] = "@(#)putchar.c	5.1 (Berkeley) %G%";
#endif not lint

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
