/*
 *	@(#)outstr_.c	5.1 (Berkeley) 11/3/86
 */

#include <stdio.h>

/* print a character string */
outstr_(s, n)
register char *s;
register long n;
{
while ( --n >= 0)
	putchar(*s++);
}
