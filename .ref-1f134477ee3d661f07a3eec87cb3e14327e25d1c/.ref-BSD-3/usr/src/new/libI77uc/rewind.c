/*
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
	if(not_legal(lunit)) err(a->aerr,101,"rewind")
	b = &units[lunit];
	if(!b->ufd && (n=fk_open(READ,SEQ,FMT,lunit)) ) err(a->aerr,n,"rewind")
	lfname = b->ufnm;
	if(!b->useek) err(a->aerr,106,"rewind")
	b->uend = NO;
	if(b->uwrt)
		if(n=t_runc(b,a->aerr)) return(n);
	rewind(b->ufd);
	return(OK);
}
