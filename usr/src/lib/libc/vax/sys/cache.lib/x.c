/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)x.c	8.1 (Berkeley) %G%";
#endif /* not lint */

main()
{
	register int i;

	for (i = 0; i < 100000; i++)
		if (vfork() == 0) {
			printf("child: %d\n", getpid());
			exit(0);
		} else {
			printf("parent: %d\n", getpid());
			wait(0);
		}
}
