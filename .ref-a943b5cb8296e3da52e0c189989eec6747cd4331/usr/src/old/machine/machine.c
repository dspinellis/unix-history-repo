/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)machine.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * machine -- print machine type
 */

main(argc,argv)
	char *argv[];
{

#ifdef vax
	printf("vax\n");
#endif
#ifdef tahoe
	printf("tahoe\n");
#endif
	exit(0);
}
