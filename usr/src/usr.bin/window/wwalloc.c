#ifndef lint
static	char *sccsid = "@(#)wwalloc.c	3.4 83/08/26";
#endif

#include "ww.h"

char **
wwalloc(nrow, ncol, size)
int nrow, ncol, size;
{
	register char *p, **pp;
	register int i;

	/* fast, call malloc only once */
	pp = (char **)
		malloc((unsigned) sizeof (char **) * nrow + size * nrow * ncol);
	if (pp == 0) {
		wwerrno = WWE_NOMEM;
		return 0;
	}
	p = (char *)&pp[nrow];
	size /= sizeof (char);		/* paranoid */
	size *= ncol;
	for (i = 0; i < nrow; i++) {
		pp[i] = p;
		p += size;
	}
	return pp;
}

wwfree(p)
register char **p;
{
	free((char *)p);
}
