#ifndef lint
static char sccsid[] = "@(#)ungetc.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>

ungetc(c, iop)
	register FILE *iop;
{
	if (c == EOF || (iop->_flag & (_IOREAD|_IORW)) == 0 ||
	    iop->_ptr == NULL || iop->_base == NULL)
		return (EOF);

	if (iop->_ptr == iop->_base)
		iop->_ptr++;

	iop->_cnt++;
	*--iop->_ptr = c;

	return (c);
}
