/* phoon - show the phase of the moon

ver  date   who remarks
--- ------- --- -------------------------------------------------------------
01A 08nov86 JP  Translated from the ratfor version of 12nov85, which itself
                  was translated from the Pascal version of 05apr79.

Copyright (C) 1986 by Jeffrey A. Poskanzer.  Permission to use, copy,
modify, and distribute this software and its documentation for any
purpose and without fee is hereby granted, provided that this copyright
notice appear in all copies and in all supporting documentation.  No
representation is made about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.

*/

static char copyright[] = "\nCopyright (C) 1986 by Jeffrey A. Poskanzer.\n";


#include <stdio.h>
#include <math.h>
#include "tws.h"


/* Global defines and declarations. */

#define SECSPERMINUTE 60
#define SECSPERHOUR (60 * SECSPERMINUTE)
#define SECSPERDAY (24 * SECSPERHOUR)

#define PI 3.14159

#define NUMLINES 23
#define ASPECTRATIO 0.5
#define MOONSTARTCOL 18
#define QUARTERLITLEN 16
#define QUARTERLITLENPLUSONE 17


/* Main program. */

main( argc, argv, envp )
int argc;
char *argv[], *envp[];
    {
    struct tws t, *twp;
    char buf[100];

    /* Figure out what date and time to use. */
    if ( argc == 1 )
	{
	/* No arguments present - use the current date and time. */
	twscopy( &t, dtwstime( ) );
	}
    else if ( argc == 2 || argc == 3 || argc == 4 )
	{
	/* One, two, or three args - use them. */
	strcpy( buf, argv[1] );
	if ( argc > 2 )
	    {
	    strcat( buf, " " );
	    strcat( buf, argv[2] );
	    if ( argc > 3 )
		{
		strcat( buf, " " );
		strcat( buf, argv[3] );
		}
	    }
	twp = dparsetime( buf );
	if ( twp == NULL || twp -> tw_flags & TW_JUNK )
	    {
	    fprintf( stderr, "illegal date/time: %s\n", buf );
	    exit( 1 );
	    }
	twscopy( &t, twp );
	}
    else
	{
	/* Too many args! */
	fprintf( stderr, "usage:  %s  [ <date/time> ]\n", argv[0] );
	exit( 1 );
	}

    /* Pseudo-randomly decide what the moon is made of, and print it. */
    if ( twclock( dtwstime( ) ) % 17 == 3 )
	putmoon( &t, "GREENCHEESE" );
    else
	putmoon( &t, "@" );

    /* All done. */
    exit( 0 );
    }


putmoon( t, atfiller )
struct tws *t;
char *atfiller;
    {
    struct tws twsanewmoon;
    long secsynodic = 29*SECSPERDAY + 12*SECSPERHOUR + 44*SECSPERMINUTE + 3;
    long secdiff, secphase;
    int atflrlen, atflridx, lin, col, midlin, qlitidx;
    float angphase, mcap, yrad, xrad, y, xright, xleft;
    int colright, colleft, i;
    char c;

    static char background[NUMLINES][47] = {
	"                 .------------.                ",
	"             .--'  o     . .   `--.            ",
	"          .-'   .    O   .       . `-.         ",
	"       .-'@   @@@@@@@   .  @@@@@      `-.      ",
	"      /@@@  @@@@@@@@@@@   @@@@@@@   .    \\     ",
	"    ./    o @@@@@@@@@@@   @@@@@@@       . \\.   ",
	"   /@@  o   @@@@@@@@@@@.   @@@@@@@   O      \\  ",
	"  /@@@@   .   @@@@@@@o    @@@@@@@@@@     @@@ \\ ",
	"  |@@@@@               . @@@@@@@@@@@@@ o @@@@| ",
	" /@@@@@  O  `.-./  .      @@@@@@@@@@@@    @@  \\",
	" | @@@@    --`-'       o     @@@@@@@@ @@@@    |",
	" |@ @@@        `    o      .  @@   . @@@@@@@  |",
	" |       @@            .-.     @@@   @@@@@@@  |",
	" \\  . o        @@@     `-'   . @@@@   @@@@  o /",
	"  |      @@   @@@@@ .           @@   .       | ",
	"  \\     @@@@  @\\@@    /  .  O    .     o   . / ",
	"   \\  o  @@     \\ \\  /         .    .       /  ",
	"    `\\     .    .\\.-.___   .      .   .-. /'   ",
	"      \\           `-'                `-' /     ",
	"       `-.   o   / |     o    O   .   .-'      ",
	"          `-.   /     .       .    .-'         ",
	"             `--.       .      .--'            ",
	"                 `------------'                " };

    static char qlits[8][16] = {
	"New Moon +     ",
	"First Quarter +",
	"Full Moon +    ",
	"Last Quarter + ",
	"First Quarter -",
	"Full Moon -    ",
	"Last Quarter - ",
	"New Moon -     " };


    /* Find the length of the atfiller string. */
    atflrlen = strlen( atfiller );

    /* Convert a new moon date from a string to a tws. */
    twscopy( &twsanewmoon, dparsetime( "05jan81 23:24:00 PST" ) );

    /* Subtract the new moon date from the desired date to get the interval
       since the new moon. */
    secdiff = twsubtract( t, &twsanewmoon );

    /* Figure out the phase - the interval since the last new moon. */
    secphase = secdiff % secsynodic;
    if ( secphase < 0L )
	secphase += secsynodic;  /* fucking mathematician language designers */
    angphase = (float) secphase / (float) secsynodic * 2.0 * PI;
    mcap = -cos( angphase );

    /* Figure out how big the moon is. */
    yrad = NUMLINES / 2.0;
    xrad = yrad / ASPECTRATIO;

    /* Figure out some other random stuff. */
    midlin = NUMLINES / 2;
    qlitidx = angphase / PI * 2.0;

    /* Now output the moon, a slice at a time. */
    atflridx = 0;
    for ( lin = 0; lin < NUMLINES; lin = lin + 1 )
	{
	/* Compute the edges of this slice. */
	y = lin + 0.5 - yrad;
	xright = xrad * sqrt( 1.0 - ( y * y ) / ( yrad * yrad ) );
	xleft = -xright;
	if ( angphase >= 0.0 && angphase < PI )
	    xleft = mcap * xleft;
	else
	    xright = mcap * xright;
	colleft = (int) (xrad + 0.5) + (int) (xleft + 0.5);
	colright = (int) (xrad + 0.5) + (int) (xright + 0.5);

	/* Now output the slice. */
	for ( i = 0; i < colleft; i++ )
	    putchar( ' ' );
	for ( col = colleft; col <= colright; col = col + 1 )
	    if ( ( c = background[lin][col] ) != '@' )
		putchar( c );
	    else
		{
		putchar( atfiller[atflridx] );
		atflridx = (atflridx + 1) % atflrlen;
		}

	/* Output the end-of-line information, if any. */
	if ( lin == midlin - 2 )
	    {
	    putchar( '\t' );
	    putchar( '\t' );
	    fputs( qlits[qlitidx], stdout );
	    }
	else if ( lin == midlin - 1)
	    {
	    putchar( '\t' );
	    putchar( '\t' );
	    putseconds( secphase % (secsynodic / 4) );
	    }
	else if ( lin == midlin )
	    {
	    putchar( '\t' );
	    putchar( '\t' );
	    fputs( qlits[qlitidx + 4], stdout );
	    }
	else if ( lin == midlin + 1 )
	    {
	    putchar( '\t' );
	    putchar( '\t' );
	    putseconds( (secsynodic - secphase) % (secsynodic / 4) );
	    }

	putchar( '\n' );
	}

    }


putseconds( secs )
long secs;
    {
    long days, hours, minutes;

    days = secs / SECSPERDAY;
    secs = secs - days * SECSPERDAY;
    hours = secs / SECSPERHOUR;
    secs = secs - hours * SECSPERHOUR;
    minutes = secs / SECSPERMINUTE;
    secs = secs - minutes * SECSPERMINUTE;

    printf( "%ld %2ld:%02ld:%02ld", days, hours, minutes, secs );
    }
