/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)version.c 4.1 10/10/80";

    /*
     *	this writes the declaration of the current time stamp
     *	onto standard output.
     *	useful for makeing Ver.c give the correct date for pi.
     */

#include	<time.h>

main()
    {
	printf( "long	createtime = %d;\n" , time(0) );
    }

