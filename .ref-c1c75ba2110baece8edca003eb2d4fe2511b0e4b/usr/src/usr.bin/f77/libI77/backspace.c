/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)backspace.c	5.2	%G%
 */

/*
 * Backspace records
 */

#include "fio.h"

static char	bksp[]	= "backspace";
char	last_char();

f_back(a)
alist	*a;
{	unit *b;
	int n,i;
	long x,y;

	lfname = NULL;
	elist = NO;
	external = YES;
	errflag = a->aerr;
	lunit = a->aunit;
	if (not_legal(lunit))
		err(errflag, F_ERUNIT, bksp)
	b= &units[lunit];
	if(!b->ufd) return(OK);
	lfname = b->ufnm;
	if(b->uend)
	{	b->uend = NO;
		clearerr(b->ufd);
		return(OK);
	}
	if((x = ftell(b->ufd)) == 0)
		return(OK);
	if(!b->useek)
		err(errflag, F_ERNOBKSP, bksp)
	if(b->uwrt && (n = t_runc(b, errflag, bksp)))	/* sets 'reading' */
		return(n);
	if(b->url)		/* direct access, purely academic */
	{	y = x%(long)b->url;
		x -= y?y:b->url;
		fseek(b->ufd,x,0);
		return(OK);
	}
	if(!b->ufmt)		/* unformatted sequential */
	{	fseek(b->ufd,-(long)sizeof(int),1);
		fread((char *)&n,sizeof(int),1,b->ufd);
		fseek(b->ufd,-(long)n-2*sizeof(int),1);
		return(OK);
	}
	if(x == 1)			/* formatted sequential */
	{	rewind(b->ufd);
		return(OK);
	}
	while (last_char(b->ufd) != '\n')	/* slow but simple */
		;
	return(OK);
}
