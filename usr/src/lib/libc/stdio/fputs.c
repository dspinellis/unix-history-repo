/* @(#)fputs.c	4.3 (Berkeley) %G% */
#include	<stdio.h>

fputs(s, iop)
register char *s;
register FILE *iop;
{
	register r = 0;
	register c;
	int unbuffered;
	char localbuf[BUFSIZ];

	unbuffered = iop->_flag & _IONBF;
	if (unbuffered) {
		iop->_flag &= ~_IONBF;
		iop->_ptr = iop->_base = localbuf;
		iop->_bufsiz = BUFSIZ;
	}

	while (c = *s++)
		r = putc(c, iop);

	if (unbuffered) {
		fflush(iop);
		iop->_flag |= _IONBF;
		iop->_base = NULL;
		iop->_bufsiz = NULL;
		iop->_cnt = 0;
	}

	return(r);
}
