#ifndef lint
static	char *sccsid = "@(#)wwputc.c	3.1 83/08/11";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
