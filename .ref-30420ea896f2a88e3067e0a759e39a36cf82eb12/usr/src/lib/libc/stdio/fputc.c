/* @(#)fputc.c	4.1 (Berkeley) %G% */
#include <stdio.h>

fputc(c, fp)
FILE *fp;
{
	return(putc(c, fp));
}
