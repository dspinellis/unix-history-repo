#ifndef lint
static	char *sccsid = "@(#)wwputc.c	2.1 83/07/30";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
