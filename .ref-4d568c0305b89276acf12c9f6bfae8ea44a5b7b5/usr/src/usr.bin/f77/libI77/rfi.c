/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rfi.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * internal (character array) i/o: read formatted, sequential and direct
 */

#include "fio.h"

extern int rd_ed(),rd_ned();
extern int z_getc(),z_rnew(),z_tab();

LOCAL
c_rfi()
{
	reading = YES;
	doed=rd_ed;
	doned=rd_ned;
	getn=z_getc;
	doend = donewrec = z_rnew;
	dorevert = z_rnew;
	dotab = z_tab;
}

s_rsfi(a) icilist *a;
{
	int n;

	c_rfi();
	if( n = c_si(a) ) return (n);
	if(pars_f()) err(errflag,F_ERFMT,"rsfio")
	fmt_bg();
	return( OK );
}

s_rdfi(a) icilist *a;
{
	int n;

	c_rfi();
	if( n = c_di(a) ) return (n);
	if(pars_f()) err(errflag,F_ERFMT,"rdfio")
	fmt_bg();
	return( OK );
}

e_rsfi()
{	int n;
	n = en_fio();
	fmtbuf = NULL;
	return(n);
}

e_rdfi()
{
	return(e_rsfi());
}
