#ifndef lint
static	char *sccsid = "@(#)wwputc.c	3.2 84/03/03";
#endif

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	(void) wwwrite(w, &c, sizeof c);
}
