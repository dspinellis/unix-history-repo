#ifndef lint
static char sccsid[] = "@(#)scanf.c	5.1 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>

scanf(fmt, args)
char *fmt;
{
	return(_doscan(stdin, fmt, &args));
}

fscanf(iop, fmt, args)
FILE *iop;
char *fmt;
{
	return(_doscan(iop, fmt, &args));
}

sscanf(str, fmt, args)
register char *str;
char *fmt;
{
	FILE _strbuf;

	_strbuf._flag = _IOREAD|_IOSTRG;
	_strbuf._ptr = _strbuf._base = str;
	_strbuf._cnt = 0;
	while (*str++)
		_strbuf._cnt++;
	_strbuf._bufsiz = _strbuf._cnt;
	return(_doscan(&_strbuf, fmt, &args));
}
