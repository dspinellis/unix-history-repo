/* @(#)fputs.c	4.1 (Berkeley) 12/21/80 */
#include	<stdio.h>

fputs(s, iop)
register char *s;
register FILE *iop;
{
	register r;
	register c;

	while (c = *s++)
		r = putc(c, iop);
	return(r);
}
