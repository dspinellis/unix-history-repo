/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)linemod.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "hp2648.h"

linemod( line )
char	*line;
{
	putchar('Z'); 
	handshake();
	putchar(ESC); 
	putchar(GRAPHIC);
	putchar(MODE);
	if ( *(line) == 's' ) {
		if ( *(++line) == 'o' ) {
			/*
			 * solid mode 1
			 */
			putchar( '1' );
			putchar( 'b' );
			goto done;
		}
		else if ( *(line) == 'h' ) {
			/*
			 * shortdashed mode 4
			 */
			putchar( '6' );
			putchar( 'b' );
			goto done;
		}
	}
	else if ( *(line) == 'd' ) {
		if ( *(++line) == 'o' && *(++line) == 't' ) {
			if ( *(++line) == 't' ) {
				/*
				 * dotted mode 2
				 */
				putchar( '7' );
				putchar( 'b' );
				goto done;
			}
			else if ( *(line) == 'd' ) {
				/*
				 * dotdashed mode 3
				 */
				putchar( '8' );
				putchar( 'b' );
				goto done;
			}
		}
	}
	else if ( *(line) == 'l' ) {
		/*
		 * longdashed mode 5
		 */
		putchar( '5' );
		putchar( 'b' );
		goto done;
	}
	putchar( '1' );				/* default to solid */
	putchar( 'b' );				/* default to solid */
done:
	putchar( 'Z' );
	handshake();
	putchar(ESC); 
	putchar(GRAPHIC);
	putchar(PLOT);
	putchar(BINARY);
	buffcount = 4;
	return;
}
