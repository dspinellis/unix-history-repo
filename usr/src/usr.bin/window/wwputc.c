#ifndef lint
static	char *sccsid = "@(#)wwputc.c	1.5 83/07/22";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
