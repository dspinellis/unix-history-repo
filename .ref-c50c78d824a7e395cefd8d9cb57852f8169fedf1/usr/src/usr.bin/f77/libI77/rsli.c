/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rsli.c	5.1	%G%
 */

/*
 * internal (character array) i/o: read sequential list
 */

#include "fio.h"
#include "lio.h"

extern int l_read(), z_getc(), z_ungetc();

s_rsli(a) icilist *a;
{
	reading = YES;
	lioproc = l_read;
	getn = z_getc;
	ungetn = z_ungetc;
	l_first = YES;
	lcount = 0;
	lquit = NO;
	return(c_li(a));
}

e_rsli()
{	fmtbuf = NULL;
	return(OK);
}
