/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)machine.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>

main()
{
	puts(MACHINE);
	exit(0);
}
