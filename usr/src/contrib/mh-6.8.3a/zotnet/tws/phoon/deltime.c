/* deltime.c - subtract date/times

ver  date   who remarks
--- ------- --- -------------------------------------------------------------
01B 15nov86 JP  Modified to use twsubtract().
01A 08nov86 JP  Written.

Copyright (C) 1986 by Jef Poskanzer.  Permission to use, copy,
modify, and distribute this software and its documentation for any
purpose and without fee is hereby granted, provided that this copyright
notice appear in all copies and in all supporting documentation.  No
representation is made about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.

*/

static char copyright[] = "\nCopyright (C) 1986 by Jef Poskanzer.\n";


#include "tws.h"
#include <stdio.h>

#define SECSPERMINUTE 60
#define SECSPERHOUR (60 * SECSPERMINUTE)
#define SECSPERDAY (24 * SECSPERHOUR)

main( argc, argv )
int argc;
char *argv[];
    {
    struct tws tws1, tws2, *twp;
    long delta, days, hours, minutes, secs;
    char *illdt = "illegal date/time: %s\n";

    if ( argc == 2 )
	{
	twp = dparsetime( argv[1] );
	if ( twp == NULL || twp -> tw_flags & TW_JUNK )
	    {
	    fprintf( stderr, illdt, argv[1] );
	    exit( 1 );
	    }
	twscopy( &tws1, twp );
	twscopy( &tws2, dtwstime( ) );
	}
    else if ( argc == 3 )
	{
	twp = dparsetime( argv[1] );
	if ( twp == NULL || twp -> tw_flags & TW_JUNK )
	    {
	    fprintf( stderr, illdt, argv[1] );
	    exit( 1 );
	    }
	twscopy( &tws1, twp );
	twp = dparsetime( argv[2] );
	if ( twp == NULL || twp -> tw_flags & TW_JUNK )
	    {
	    fprintf( stderr, illdt, argv[2] );
	    exit( 1 );
	    }
	twscopy( &tws2, twp );
	}
    else
	{
	fprintf( stderr, "usage:  %s  <time>  [ <time2> ]\n", argv[0] );
	exit( 1 );
	}
    
    delta = twsubtract( &tws2, &tws1 );
    if ( delta < 0 )
	{
	printf( "-" );
	delta = -delta;
	}

    days = delta / SECSPERDAY;
    delta = delta - days * SECSPERDAY;
    hours = delta / SECSPERHOUR;
    delta = delta - hours * SECSPERHOUR;
    minutes = delta / SECSPERMINUTE;
    delta = delta - minutes * SECSPERMINUTE;
    secs = delta;

    printf( "%ld %2ld:%02ld:%02ld\n", days, hours, minutes, secs );

    exit( 0 );
    }
