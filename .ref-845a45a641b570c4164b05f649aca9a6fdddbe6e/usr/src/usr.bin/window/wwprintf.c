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

/*VARARGS2*/
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
