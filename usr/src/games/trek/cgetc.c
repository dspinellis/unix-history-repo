/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cgetc.c	5.1 (Berkeley) %G%";
#endif not lint

# include	<stdio.h>

char	cgetc(i)
int	i;
{
	return ( getchar() );
}
