#ifndef lint
static	char *sccsid = "@(#)wwmisc.c	3.1 83/08/11";
#endif

#include "ww.h"

char *
unctrl(c)
register char c;
{
	static char buf[5];
	register char *p = buf;

	if (c == DEL) {
		*p++ = '^';
		*p++ = '?';
	} else if (c < ' ') {
		*p++ = '^';
		*p++ = c + '@';
	} else if (c > DEL) {
		*p++ = '\\';
		*p++ = (c >> 6 & 3) + '0';
		*p++ = (c >> 3 & 7) + '0';
		*p++ = (c & 7) + '0';
	} else
		*p++ = c;
	*p = 0;
	return buf;
}
