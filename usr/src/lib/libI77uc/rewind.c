/*
char id_rewind[] = "@(#)rewind.c	1.2";
 *
 * rewind.c  -  f77 file rewind
 */

#include "fio.h"

f_rew(a) alist *a;
{	int n;
	unit *b;

	lfname = NULL;
	elist = NO;
	external = YES;			/* for err */
	lunit = a->aunit;
	errflag = a->aerr;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,"rewind")
	b = &units[lunit];
	if(!b->ufd && (n=fk_open(READ,SEQ,FMT,(ftnint)lunit)) )
		err(errflag,n,"rewind")
	lfname = b->ufnm;
	if(!b->useek) err(errflag,F_ERNOBKSP,"rewind")
	b->uend = NO;
	if(b->uwrt)
		if(n=t_runc(b,errflag)) return(n);
	rewind(b->ufd);
	return(OK);
}
