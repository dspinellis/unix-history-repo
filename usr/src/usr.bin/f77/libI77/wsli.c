/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wsli.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * internal (character array) i/o: write sequential list
 */

#include "fio.h"
#include "lio.h"

extern int l_write(), z_putc(), z_wnew();

s_wsli(a) icilist *a;
{
	reading = NO;
	putn = z_putc;
	lioproc = l_write;
	line_len = a->icirlen;
	return(c_li(a));
}

e_wsli()
{	fmtbuf = NULL;
	reclen = recpos;
	return(z_wnew());
}
