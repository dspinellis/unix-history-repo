/* @(#)fputc.c	4.1 (Berkeley) 12/21/80 */
#include <stdio.h>

fputc(c, fp)
FILE *fp;
{
	return(putc(c, fp));
}
