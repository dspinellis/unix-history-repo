/* @(#)fgetc.c	4.1 (Berkeley) 12/21/80 */
#include <stdio.h>

fgetc(fp)
FILE *fp;
{
	return(getc(fp));
}
