/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)dolio.c	5.4 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * list directed and namelist i/o common routines
 */

#include "fio.h"
#include "lio.h"


c_le(a,flg) cilist *a;
{	int n;
	lfname = NULL;
	elist = NO;
	sequential=external=YES;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,fmtbuf)
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flg,SEQ,FMT,(ftnint)lunit)))
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
