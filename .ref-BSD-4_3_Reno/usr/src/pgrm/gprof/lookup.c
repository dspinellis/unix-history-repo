/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lookup.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include "gprof.h"

    /*
     *	look up an address in a sorted-by-address namelist
     *	    this deals with misses by mapping them to the next lower 
     *	    entry point.
     */
nltype *
nllookup( address )
    unsigned long	address;
{
    register long	low;
    register long	middle;
    register long	high;
#   ifdef DEBUG
	register int	probes;

	probes = 0;
#   endif DEBUG
    for ( low = 0 , high = nname - 1 ; low != high ; ) {
#	ifdef DEBUG
	    probes += 1;
#	endif DEBUG
	middle = ( high + low ) >> 1;
	if ( nl[ middle ].value <= address && nl[ middle+1 ].value > address ) {
#	    ifdef DEBUG
		if ( debug & LOOKUPDEBUG ) {
		    printf( "[nllookup] %d (%d) probes\n" , probes , nname-1 );
		}
#	    endif DEBUG
	    return &nl[ middle ];
	}
	if ( nl[ middle ].value > address ) {
	    high = middle;
	} else {
	    low = middle + 1;
	}
    }
    fprintf( stderr , "[nllookup] binary search fails???\n" );
    return 0;
}

arctype *
arclookup( parentp , childp )
    nltype	*parentp;
    nltype	*childp;
{
    arctype	*arcp;

    if ( parentp == 0 || childp == 0 ) {
	fprintf( "[arclookup] parentp == 0 || childp == 0\n" );
	return 0;
    }
#   ifdef DEBUG
	if ( debug & LOOKUPDEBUG ) {
	    printf( "[arclookup] parent %s child %s\n" ,
		    parentp -> name , childp -> name );
	}
#   endif DEBUG
    for ( arcp = parentp -> children ; arcp ; arcp = arcp -> arc_childlist ) {
#	ifdef DEBUG
	    if ( debug & LOOKUPDEBUG ) {
		printf( "[arclookup]\t arc_parent %s arc_child %s\n" ,
			arcp -> arc_parentp -> name ,
			arcp -> arc_childp -> name );
	    }
#	endif DEBUG
	if ( arcp -> arc_childp == childp ) {
	    return arcp;
	}
    }
    return 0;
}
