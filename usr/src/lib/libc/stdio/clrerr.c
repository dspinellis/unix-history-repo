#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)clrerr.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#undef	clearerr

clearerr(iop)
	register FILE *iop;
{
	iop->_flag &= ~(_IOERR|_IOEOF);
}
