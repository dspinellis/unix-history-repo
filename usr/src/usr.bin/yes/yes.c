/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)yes.c	5.4 (Berkeley) %G%";
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
