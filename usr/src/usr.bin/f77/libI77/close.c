/*
char id_close[] = "@(#)close.c	1.1";
 *
 * close.c  -  f77 file close, flush, exit routines
 */

#include "fio.h"

#define FROM_OPEN	'\1'

f_clos(a) cllist *a;
{	unit *b;
	lfname = NULL;
	elist = NO;
	external = YES;
	errflag = a->cerr;
	lunit = a->cunit;
	if(not_legal(lunit)) err(errflag,101,"close");
	if(lunit==STDERR && (!a->csta || *a->csta != FROM_OPEN))
		err(errflag,101,"can't close stderr");
	b= &units[lunit];
	if(!b->ufd) err(errflag,114,"close");
	if(a->csta)
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
			if(b->uwrt) t_runc(b,errflag);
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
	xx.csta=NULL;
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
	FILE *F = units[*u].ufd;
	if(F)
		return(fflush(F));
	else
		return(114);
}
