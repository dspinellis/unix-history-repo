/*
char id_close[] = "@(#)close.c	1.6";
 *
 * close.c  -  f77 file close, flush, exit routines
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
	if(not_legal(lunit)) err(errflag,F_ERUNIT,clse);
	if(lunit==STDERR && (!a->csta || *a->csta != FROM_OPEN[0]))
		err(errflag,F_ERUNIT,"can't close stderr");
	b= &units[lunit];
	if(!b->ufd) err(errflag,F_ERNOPEN,clse);
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

ftnint
flush_(u) ftnint *u;
{
	FILE *F;

	if(not_legal(*u))
		return(F_ERUNIT);
	F = units[*u].ufd;
	if(F)
		return(fflush(F));
	else
		return(F_ERNOPEN);
}
