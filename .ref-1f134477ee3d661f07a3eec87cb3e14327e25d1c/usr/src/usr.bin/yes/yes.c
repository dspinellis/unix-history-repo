/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1987, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)yes.c	8.1 (Berkeley) %G%";
#endif /* not lint */

main(argc, argv)
	int argc;
	char **argv;
{
	if (argc > 1)
		for(;;)
			puts(argv[1]);
	else for (;;)
		puts("y");
}
