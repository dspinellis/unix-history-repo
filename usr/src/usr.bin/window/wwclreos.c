#ifndef lint
static	char *sccsid = "@(#)wwclreos.c	3.2 83/08/11";
#endif

#include "ww.h"

wwclreos(w, line, col)
register struct ww *w;
{
	register i;

	wwclreol(w, line, col);
	for (i = line + 1; i < w->ww_nline; i++)
		wwclreol(w, i, 0);
}
