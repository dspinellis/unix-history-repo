/* @(#)fputs.c	4.2 (Berkeley) %G% */
#include	<stdio.h>

fputs(s, iop)
register char *s;
register FILE *iop;
{
	register r = 0;
	register c;

	while (c = *s++)
		r = putc(c, iop);
	return(r);
}
