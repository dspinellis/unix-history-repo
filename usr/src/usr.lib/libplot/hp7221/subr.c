/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.1 (Berkeley) 5/7/85";
#endif not lint

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
