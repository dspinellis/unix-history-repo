/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)yycopy.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include	"whoami.h"
#include	"0.h"
#include 	"tree_ty.h"		/* must be included for yy.h */
#include	"yy.h"

OYcopy ()
    {
	register int	*r0 = ((int *) & OY);
	register int	*r1 = ((int *) & Y);
	register int	r2 = ( sizeof ( struct yytok ) ) / ( sizeof ( int ) );

	do
	    {
		* r0 ++ = * r1 ++ ;
	    }
	    while ( -- r2 > 0 );
    }
