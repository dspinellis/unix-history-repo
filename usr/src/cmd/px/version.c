/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)version.c 1.2 3/6/81";

    /*
     *	this writes the declaration of the current time stamp
     *	onto standard output.
     *	useful for making Version.c to give the creation date for px.
     */

extern long time();

main()
{
	printf( "long	createtime = %D;\n" , time(0) );
}
