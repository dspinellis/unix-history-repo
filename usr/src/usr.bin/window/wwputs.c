#ifndef lint
static	char *sccsid = "@(#)wwputs.c	1.5 83/07/22";
#endif

#include "ww.h"

wwputs(s, w)
register char *s;
struct ww *w;
{
	register char *p = s;

	while (*p++)
		;
	wwwrite(w, s, p - s - 1);
}
