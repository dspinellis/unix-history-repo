#include <stdio.h>

peekc(ib)
FILE *ib;
{
	register c;

	c = getc(ib);
	ungetc(c,ib);
	return(c);
}
