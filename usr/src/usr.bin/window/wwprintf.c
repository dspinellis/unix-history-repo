/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)wwprintf.c	3.7 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <varargs.h>

/*VARARGS2*/
wwprintf(w, fmt, va_alist)
struct ww *w;
char *fmt;
va_dcl
{
#include <stdio.h>
	struct _iobuf _wwbuf;
	char buf[1024];
	va_list ap;

	/*
	 * A danger is that when buf overflows, _flsbuf() will be
	 * called automatically.  It doesn't check for _IOSTR.
	 * We set the file descriptor to -1 so no actual io will be done.
	 */
	_wwbuf._flag = _IOWRT+_IOSTRG;
	_wwbuf._base = _wwbuf._ptr = buf;
	_wwbuf._cnt = sizeof buf;
	_wwbuf._file = -1;			/* safe */
	va_start(ap);
	_doprnt(fmt, ap, &_wwbuf);
	va_end(ap);
	(void) wwwrite(w, buf, _wwbuf._ptr - buf);
}
