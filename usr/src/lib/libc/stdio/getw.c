#ifndef lint
static char sccsid[] = "@(#)getw.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>

getw(iop)
register FILE *iop;
{
	register i;
	register char *p;
	int w;

	p = (char *)&w;
	for (i=sizeof(int); --i>=0;)
		*p++ = getc(iop);
	if (feof(iop))
		return(EOF);
	return(w);
}
