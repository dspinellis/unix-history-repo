/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)wfi.c	5.1	6/7/85
 */

/*
 * internal (character array) i/o: write formatted, sequential and direct
 */

#include "fio.h"

extern int w_ed(),w_ned();
extern int z_wnew(), z_putc(), z_tab();

LOCAL
c_wfi()
{
	reading = NO;
	doed=w_ed;
	doned=w_ned;
	putn=z_putc;
	doend = donewrec = z_wnew;
	dorevert = z_wnew;
	dotab = z_tab;
}

s_wsfi(a) icilist *a;
{
	int n;

	c_wfi();
	if( n = c_si(a) ) return (n);
	if(pars_f()) err(errflag,F_ERFMT,"wsfio")
	fmt_bg();
	return( OK );
}

s_wdfi(a) icilist *a;
{
	int n;

	c_wfi();
	if( n = c_di(a) ) return (n) ;
	if(pars_f()) err(errflag,F_ERFMT,"wdfio")
	fmt_bg();
	return( OK );
}

e_wsfi()
{
	int n;
	n = en_fio();
	fmtbuf = NULL;
	return(n);
}


e_wdfi()
{
	return(e_wsfi());
}
