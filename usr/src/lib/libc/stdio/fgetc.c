#ifndef lint
static char sccsid[] = "@(#)fgetc.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>

fgetc(fp)
FILE *fp;
{
	return(getc(fp));
}
