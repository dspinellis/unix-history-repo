#ifndef lint
static	char *sccsid = "@(#)wwputc.c	1.4 83/07/19";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
