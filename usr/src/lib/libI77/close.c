/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)close.c	5.3	2/25/90
 */

/*
 * f_clos(): f77 file close
 * t_runc(): truncation
 * f_exit(): I/O library exit routines
 */

#include "fio.h"

static char FROM_OPEN[] =	"\2";
static char clse[]	=	"close";

f_clos(a) cllist *a;
{	unit *b;
	int n;

	lfname = NULL;
	elist = NO;
	external = YES;
	errflag = a->cerr;
	lunit = a->cunit;
	if(not_legal(lunit)) return(OK);
	if(lunit==STDERR && (!a->csta || *a->csta != FROM_OPEN[0]))
		err(errflag,F_ERUNIT,"can't close stderr");
	b= &units[lunit];
	if(!b->ufd) return(OK);
	if(a->csta && *a->csta != FROM_OPEN[0])
		switch(lcase(*a->csta))
		{
	delete:
		case 'd':
			fclose(b->ufd);
			if(b->ufnm) unlink(b->ufnm); /*SYSDEP*/
			break;
		default:
	keep:
		case 'k':
			if(b->uwrt && (n=t_runc(b,errflag,clse))) return(n);
			fclose(b->ufd);
			break;
		}
	else if(b->uscrtch) goto delete;
	else goto keep;
	if(b->ufnm) free(b->ufnm);
	b->ufnm=NULL;
	b->ufd=NULL;
	return(OK);
}

f_exit()
{
	ftnint lu, dofirst = YES;
	cllist xx;
	xx.cerr=1;
	xx.csta=FROM_OPEN;
	for(lu=STDOUT; (dofirst || lu!=STDOUT); lu = ++lu % MXUNIT)
	{
		xx.cunit=lu;
		f_clos(&xx);
		dofirst = NO;
	}
}

t_runc (b, flg, str)
unit	*b;
ioflag	flg;
char	*str;
{
	long	loc;

	if (b->uwrt)
		fflush (b->ufd);
	if (b->url || !b->useek || !b->ufnm)
		return (OK);	/* don't truncate direct access files, etc. */
	loc = ftell (b->ufd);
	if (truncate (b->ufnm, loc) != 0)
		err (flg, errno, str)
	if (b->uwrt && ! nowreading(b))
		err (flg, errno, str)
	return (OK);
}
