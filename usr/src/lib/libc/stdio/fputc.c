/* @(#)fputc.c	4.2 (Berkeley) %G% */
#include <stdio.h>

fputc(c, fp)
register FILE *fp;
{
	return(putc(c, fp));
}
