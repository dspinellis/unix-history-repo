/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_dfe.c	5.3	%G%
 */

/*
 * direct formatted external i/o - common read/write routines
 */

#include "fio.h"

c_dfe(a,flg,str) cilist *a; char *str;
{	int n;
	sequential = NO;
	external = YES;
	formatted = FORMATTED;
	lfname = NULL;
	elist = NO;
	cursor=scale=recpos=reclen=0;
	radix = 10;
	signit = YES;
	fmtbuf = a->cifmt;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,str);
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flg,DIR,FMT,(ftnint)lunit)))
		err(errflag,n,str)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if(!curunit->ufmt) err(errflag,F_ERNOFIO,str)
	if(!curunit->useek || !curunit->url) err(errflag,F_ERNODIO,str)
	recnum = a->cirec - 1;
	fseek(cf, (long)curunit->url * recnum, 0);
	cblank = curunit->ublnk;
	cplus = NO;
	return(OK);
}

y_tab()
{	int n;
	if(curunit->url==1)
	{
		if(cursor < 0 && -cursor > ftell(cf)) rewind(cf);
		else	fseek(cf,(long)cursor,1);
		return(cursor=0);
	}
	else
	{	if(reclen < recpos) reclen = recpos;
		if((recpos + cursor) < 0) cursor = -recpos;	/* BOR */
		n = reclen - recpos;		/* n >= 0 */
		if(!reading && (cursor-n) > 0)
		{	recpos = reclen;
			cursor -= n;
			fseek(cf,(long)n,1);
			while(cursor--) if(n=(*putn)(' ')) return(n);
			return(cursor=0);
		}
		recpos += cursor;
		if(recpos >= curunit->url) err(errflag,F_EREREC,"dfe")
	}
	fseek(cf,(long)cursor,1);
	return(cursor=0);
}
