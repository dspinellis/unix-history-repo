/* parsetime.c - parse a date/time and display the results

ver  date   who remarks
--- ------- --- -------------------------------------------------------------
01A 15nov86 JP  Written.

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


main( argc, argv )
int argc;
char *argv[];
    {
    char buf[200];
    int i;
    struct tws *twp;

    strcpy( buf, "" );
    for ( i = 1; i < argc; i++ )
	{
	if ( i > 1 )
	    strcat( buf, " " );
	strcat( buf, argv[i] );
	}

    twp = dparsetime( buf );
    if ( twp == NULL )
	{
	fprintf( stderr, "illegal date/time: %s\n", buf );
	exit( 1 );
	}

    printf( "dparsetime( \"%s\" ):\n", buf );
    printf( "  tw_sec = %d\n", twp->tw_sec );
    printf( "  tw_min = %d\n", twp->tw_min );
    printf( "  tw_hour = %d\n", twp->tw_hour );
    printf( "  tw_mday = %d\n", twp->tw_mday );
    printf( "  tw_mon = %d\n", twp->tw_mon );
    printf( "  tw_year = %d\n", twp->tw_year );
    printf( "  tw_wday = %d\n", twp->tw_wday );
    printf( "  tw_yday = %d\n", twp->tw_yday );
    printf( "  tw_zone = %d\n", twp->tw_zone );
    printf( "  tw_clock = %ld\n", twp->tw_clock );
    printf( "  tw_flags = %d (0x%04x)\n", twp->tw_flags, twp->tw_flags );
    printf( "\n" );
    printf( "dasctime: %s\n", dasctime( twp, 0 ) );

    exit( 0 );
    }
