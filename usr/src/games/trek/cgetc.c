/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cgetc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

# include	<stdio.h>

char	cgetc(i)
int	i;
{
	return ( getchar() );
}
