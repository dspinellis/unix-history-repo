/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)version.c	5.1 (Berkeley) %G%";
#endif not lint

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
