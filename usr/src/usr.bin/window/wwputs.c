#ifndef lint
static	char *sccsid = "@(#)wwputs.c	3.1 83/08/11";
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
