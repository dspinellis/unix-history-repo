/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwprintf.c	3.11 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <varargs.h>

wwprintf(w, fmt, va_alist)
struct ww *w;
char *fmt;
va_dcl
{
	char buf[1024];
	va_list ap;

	va_start(ap);
	/* buffer can overflow */
	(void) wwwrite(w, buf, vsprintf(buf, fmt, ap));
	va_end(ap);
}

/*
wwprintf(w, fmt, args)
struct ww *w;
char *fmt;
{
	_doprnt(fmt, &args, w);
	return 0;
}

_strout(count, string, adjust, file, fillch)
register char *string;
register count;
int adjust;
register struct ww *file;
{
	while (adjust < 0) {
		if (*string=='-' && fillch=='0') {
			wputc(*string++, file);
			count--;
		}
		wputc(fillch, file);
		adjust++;
	}
	while (--count>=0)
		wputc(*string++, file);
	while (adjust) {
		wputc(fillch, file);
		adjust--;
	}
}
*/
