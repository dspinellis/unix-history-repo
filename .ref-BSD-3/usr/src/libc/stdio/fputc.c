#include <stdio.h>

fputc(c, fp)
FILE *fp;
{
	return(putc(c, fp));
}
