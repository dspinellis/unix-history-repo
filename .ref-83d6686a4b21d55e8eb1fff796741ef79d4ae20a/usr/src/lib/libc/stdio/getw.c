#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getw.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

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
