#ifndef lint
static char sccsid[] = "@(#)wwprintf.c	3.5 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"

wwprintf(w, fmt, args)
struct ww *w;
char *fmt;
{
#include <stdio.h>
	struct _iobuf _wwbuf;
	char buf[1024];

	/*
	 * A danger is that when buf overflows, _flsbuf() will be
	 * called automatically.  It doesn't check for _IOSTR.
	 * We set the file descriptor to -1 so no actual io will be done.
	 */
	_wwbuf._flag = _IOWRT+_IOSTRG;
	_wwbuf._base = _wwbuf._ptr = buf;
	_wwbuf._cnt = sizeof buf;
	_wwbuf._file = -1;			/* safe */
	_doprnt(fmt, &args, &_wwbuf);
	(void) wwwrite(w, buf, _wwbuf._ptr - buf);
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
