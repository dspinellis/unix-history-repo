#ifndef lint
static	char *sccsid = "@(#)wwalloc.c	3.2 83/08/11";
#endif

#include "ww.h"

char **
wwalloc(nrow, ncol, size)
{
	register char **p;
	register int i;

	p = (char **) malloc((unsigned) nrow * sizeof (char *));
	if (p == 0)
		return 0;
	for (i = 0; i < nrow; i++) {
		p[i] = malloc((unsigned) ncol * size);
		if (p[i] == 0) {
			wwfree(p, i);
			return 0;
		}
	}
	return p;
}

wwfree(p, nrow)
register char **p;
{
	register int i;

	if (p == 0)
		return;
	for (i = 0; i < nrow; i++)
		free(p[i]);
	free((char *)p);
}
