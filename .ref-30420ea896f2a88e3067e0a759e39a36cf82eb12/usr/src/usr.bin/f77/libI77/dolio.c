/*
char id_dolio[] = "@(#)dolio.c	1.2";
 *
 * list directed i/o common routines
 */

#include "fio.h"
#include "lio.h"


c_le(a,flag) cilist *a;
{	int n;
	lfname = NULL;
	elist = NO;
	sequential=external=formatted= LISTDIRECTED;
	fmtbuf = "ext list io";
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,fmtbuf)
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flag,SEQ,FMT,(ftnint)lunit)))
		err(errflag,n,fmtbuf)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	scale=recpos=cursor=0;
	cplus=cblank=NO;
	if(!curunit->ufmt) err(errflag,F_ERNOFIO,fmtbuf)
	if(curunit->url) err(errflag,F_ERNOSIO,fmtbuf)
	return(OK);
}

do_lio(type,number,ptr,len) ftnint *number,*type; flex *ptr; ftnlen len;
{
	return((*lioproc)(number,ptr,len,*type));
}
