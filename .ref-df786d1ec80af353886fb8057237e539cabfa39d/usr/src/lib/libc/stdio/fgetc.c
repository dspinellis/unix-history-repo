/* @(#)fgetc.c	4.1 (Berkeley) %G% */
#include <stdio.h>

fgetc(fp)
FILE *fp;
{
	return(getc(fp));
}
