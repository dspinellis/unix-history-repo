#ifndef lint
static	char *sccsid = "@(#)wwputs.c	1.1 83/07/12";
#endif

#include "ww.h"

wwputs(s, w)
	register char *s;
	register struct ww *w;
{
	while (*s)
		wwputc(*s++, w);
}
