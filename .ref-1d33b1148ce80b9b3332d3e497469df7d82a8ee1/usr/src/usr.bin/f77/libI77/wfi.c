/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wfi.c	5.2 (Berkeley) %G%";
#endif /* not lint */

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
