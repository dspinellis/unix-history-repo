/*
char id_endfile[] = "@(#)endfile.c	1.8";
 *
 * endfile
 */

#include "fio.h"

static char	endf[]	= "endfile";

f_end (a)
alist	*a;
{
	unit	*b;

	lfname = NULL;
	elist = NO;
	errflag = a->aerr;
	lunit = a->aunit;
	if (not_legal(lunit))
		err (errflag, F_ERUNIT, endf)
	b = &units[lunit];
	if (!b->ufd)
		err (errflag, F_ERNOPEN, endf)
	if (b->uend)
		return(0);
	lfname = b->ufnm;
	b->uend = YES;
	return ( t_runc (b, errflag, endf) );
}
