/* Copyright (c) 1979 Regents of the University of California */

#ifndef lint
static	char sccsid[] = "@(#)yycopy.c 1.4 %G%";
#endif

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
