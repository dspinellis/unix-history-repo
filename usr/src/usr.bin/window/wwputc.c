#ifndef lint
static	char *sccsid = "@(#)wwputc.c	1.2 83/07/17";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	return wwwrite(w, &c, sizeof c);
}
