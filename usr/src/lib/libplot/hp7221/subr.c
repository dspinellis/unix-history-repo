/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "hp7221.h"

putMBP( x, y )
    int		x,	y;
{
    int		chr;

    chr = ( x >> 10 ) & 017;
    chr|= 0140;
    putchar( chr );
    chr = ( x >> 4 ) & 077;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    chr = ( y >> 12 ) & 03;
    chr|= ( x << 2  ) & 071;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    chr = ( y >> 6 ) & 077;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    chr = ( y ) & 077;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    return;
}

putMBN( i )
    int		i;
{
    int		chr;

    chr = ( i>>12 ) & 07;
    chr|= 0140;
    putchar( chr );
    chr = ( i>>6 ) & 077;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    chr = i & 077;
    if ( chr < 32 ) {
	chr += 64;
    }
    putchar( chr );
    return;
}

putSBN( i )
    int		i;
{
    i &= 077;
    if ( i < 32 ) {
	i += 64;
    }
    putchar( i );
    return;
}
