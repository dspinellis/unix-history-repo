/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fprintf.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include	<stdio.h>

fprintf(iop, fmt, args)
register FILE *iop;
char *fmt;
{
	char localbuf[BUFSIZ];

	if (iop->_flag & _IONBF) {
		iop->_flag &= ~_IONBF;
		iop->_ptr = iop->_base = localbuf;
		iop->_bufsiz = BUFSIZ;
		_doprnt(fmt, &args, iop);
		fflush(iop);
		iop->_flag |= _IONBF;
		iop->_base = NULL;
		iop->_bufsiz = NULL;
		iop->_cnt = 0;
	} else
		_doprnt(fmt, &args, iop);
	return(ferror(iop)? EOF: 0);
}
