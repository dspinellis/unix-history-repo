#ifndef lint
static char sccsid[] = "@(#)sprintf.c	5.1 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>

char *sprintf(str, fmt, args)
char *str, *fmt;
{
	FILE _strbuf;

	_strbuf._flag = _IOWRT+_IOSTRG;
	_strbuf._ptr = str;
	_strbuf._cnt = 32767;
	_doprnt(fmt, &args, &_strbuf);
	putc('\0', &_strbuf);
	return(str);
}
