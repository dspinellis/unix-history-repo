#ifndef lint
static	char *sccsid = "@(#)tttermcap.c	3.1 83/08/17";
#endif

#include "ww.h"
#include "tt.h"

char *tgetstr();

tt_pc(c)
{
	putchar(c);
}

tt_sc(c)
{
	*tt_strp++ = c;
}

char *
tt_xgetstr(str)
char *str;
{
	char buf[100];
	char *bufp = buf;

	str = tgetstr(str, &bufp);
	if (str == 0)
		return 0;
	str = tt_strp;
	tputs(buf, 1, tt_sc);
	tt_sc(0);
	return str;
}
