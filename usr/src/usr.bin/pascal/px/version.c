/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)version.c 1.1 %G%";

    /*
     *	this writes the declaration of the current time stamp
     *	onto standard output.
     *	useful for making Version.c to give the creation date for px.
     */

#include	<time.h>

main()
{
	printf( "long	createtime = %d;\n" , time(0) );
}
