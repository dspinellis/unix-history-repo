/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rsfe.c	5.2	7/30/85
 */

/*
 * read sequential formatted external
 */

#include "fio.h"

extern int rd_ed(),rd_ned();
int x_rnew(),x_getc(),x_tab();

LOCAL char rsfe[] = "read sfe";

s_rsfe(a) cilist *a; /* start */
{	int n;
	reading = YES;
	sequential = YES;
	if(n=c_sfe(a,READ,SEQ,rsfe)) return (n);
	if(curunit->url) err(errflag,F_ERNOSIO,rsfe)
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, rsfe)
	getn= x_getc;
	doed= rd_ed;
	doned= rd_ned;
	donewrec = dorevert = doend = x_rnew;
	dotab = x_tab;
	if(pars_f()) err(errflag,F_ERFMT,rsfe)
	fmt_bg();
	return(OK);
}

LOCAL
x_rnew()			/* find next record */
{	int ch;
	if(curunit->uend)
		return(EOF);
	while((ch=getc(cf))!='\n' && ch!=EOF);
	if(feof(cf))
	{	curunit->uend = YES;
		if (recpos==0) return(EOF);
	}
	cursor=recpos=reclen=0;
	return(OK);
}

LOCAL
x_getc()
{	int ch;
	if(curunit->uend) return(EOF);
	if((ch=getc(cf))!=EOF && ch!='\n')
	{	recpos++;
		return(ch);
	}
	if(ch=='\n')
	{	ungetc(ch,cf);
		return(ch);
	}
	if(feof(cf)) curunit->uend = YES;
	return(EOF);
}

e_rsfe()
{	int n;
	n=en_fio();
	fmtbuf=NULL;
	return(n);
}
