/*
 *	"@(#)abort_.c	1.2"
 */

#include <stdio.h>

#if	pdp11
abort_()
{
	fprintf(stderr, "Fortran abort routine called\n");
	f_exit();
	_cleanup();
	abort();
}
#else	vax
abort_(msg,len)
char *msg; int len;
{
	fprintf(stderr, "abort: ");
	if (nargs()) while (len-- > 0) fputc(*msg++, stderr);
	else fprintf(stderr, "called");
	fputc('\n', stderr);
	f77_abort();
}
#endif	vax
