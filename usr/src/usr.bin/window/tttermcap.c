#ifndef lint
static	char *sccsid = "@(#)tttermcap.c	3.3 84/03/03";
#endif

#include "tt.h"

char *tgetstr();

tttputc(c)
{
	ttputc(c);
}

ttxputc(c)
{
	*tt_strp++ = c;
}

char *
ttxgetstr(str)
char *str;
{
	char buf[100];
	char *bufp = buf;

	str = tgetstr(str, &bufp);
	if (str == 0)
		return 0;
	str = tt_strp;
	tputs(buf, 1, ttxputc);
	ttxputc(0);
	return str;
}
