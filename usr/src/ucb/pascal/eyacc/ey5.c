/*
 * Copyright (c) 1979 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ey5.c	5.1 (Berkeley) 4/29/85";
#endif not lint

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

system(){
  error( "The function \"system\" is called" );
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
