#ifndef lint
static	char *sccsid = "@(#)wwputc.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
