    /* Copyright (c) 1979 Regents of the University of California */

    /*
     *	this writes the declaration of the character string version
     *	onto standard output.
     *	useful for makeing Version.c give the correct date for pi.
     */

#include	<time.h>

char		*ctime();

long		clock;
char		*cstring;

main()
    {
	time( &clock );
	cstring = ctime( &clock );
	cstring[ 24 ] = '\0';
	printf( "char	version[] = \"%s\";\n" , cstring );
    }

