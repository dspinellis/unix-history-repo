/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)ey5.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/* fake portable I/O routines, for those
    sites so backward as to not have the
     port. library */
/* munged for standard i/o library: peter and louise 5 may 80 */
#include <stdio.h>

FILE *cin, *cout;

FILE *copen( s, c )
    char *s;
    char c;
  {
  FILE *f;

	  if( c == 'r' ){
	    f = fopen( s, "r" );
  } else  if( c == 'a' ){
	    f = fopen( s, "a" );
	    fseek( f, 0L, 2 );
  } else {  /* c == w */
	    f = fopen( s, "w" );
  }

  return( f );
  }

cflush(x) FILE *x; { /* fake! sets file to x */
  fflush( cout );
  cout = x;
  }

cclose(i) FILE *i; {
  fclose(i);
  }

cexit(i){
  fflush( cout );
  if ( i != 0 ) {
    abort();
  }
  exit(i);
  }
