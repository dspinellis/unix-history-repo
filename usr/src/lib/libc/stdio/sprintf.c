/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sprintf.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

sprintf(str, fmt, args)
	char *str, *fmt;
	int args;
{
	FILE _strbuf;
	int len;

	_strbuf._flag = _IOWRT+_IOSTRG;
	_strbuf._ptr = str;
	_strbuf._cnt = 32767;
	len = _doprnt(fmt, &args, &_strbuf);
	*_strbuf._ptr = 0;
	return(len);
}
