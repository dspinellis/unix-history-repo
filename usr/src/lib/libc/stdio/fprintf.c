/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fprintf.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

fprintf(iop, fmt, args)
	register FILE *iop;
	char *fmt;
	int args;
{
	int len;
	char localbuf[BUFSIZ];

	if (iop->_flag & _IONBF) {
		iop->_flag &= ~_IONBF;
		iop->_ptr = iop->_base = localbuf;
		iop->_bufsiz = BUFSIZ;
		len = _doprnt(fmt, &args, iop);
		fflush(iop);
		iop->_flag |= _IONBF;
		iop->_base = NULL;
		iop->_bufsiz = NULL;
		iop->_cnt = 0;
	} else
		len = _doprnt(fmt, &args, iop);
	return(ferror(iop) ? EOF : len);
}
