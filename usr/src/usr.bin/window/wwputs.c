#ifndef lint
static	char *sccsid = "@(#)wwputs.c	3.2 84/03/03";
#endif

#include "ww.h"

wwputs(s, w)
register char *s;
struct ww *w;
{
	register char *p = s;

	while (*p++)
		;
	(void) wwwrite(w, s, p - s - 1);
}
