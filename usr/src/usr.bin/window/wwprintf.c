#ifndef lint
static	char *sccsid = "@(#)wwprintf.c	3.1 83/08/11";
#endif

#include "ww.h"

/*VARARGS2*/
wwprintf(w, fmt, args)
struct ww *w;
char *fmt;
{
	struct _iobuf _wwbuf;
	static char buf[1024];

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
	return wwwrite(w, buf, _wwbuf._ptr - buf);
}
