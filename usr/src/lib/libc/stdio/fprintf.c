/* @(#)fprintf.c	4.3 (Berkeley) %G% */
#include	<stdio.h>

fprintf(iop, fmt, args)
FILE *iop;
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
