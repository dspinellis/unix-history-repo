#ifndef lint
static	char *sccsid = "@(#)wwputs.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

wwputs(s, w)
register char *s;
struct ww *w;
{
	register char *p = s;

	while (*p++)
		;
	return wwwrite(w, s, p - s - 1);
}
