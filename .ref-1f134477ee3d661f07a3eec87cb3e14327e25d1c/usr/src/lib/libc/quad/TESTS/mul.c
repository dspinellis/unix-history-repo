/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mul.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main()
{
	union { long long q; unsigned long v[2]; } a, b, m;
	char buf[300];
	extern long long __muldi3(long long, long long);

	for (;;) {
		printf("> ");
		if (fgets(buf, sizeof buf, stdin) == NULL)
			break;
		if (sscanf(buf, "%lu:%lu %lu:%lu",
			    &a.v[0], &a.v[1], &b.v[0], &b.v[1]) != 4 &&
		    sscanf(buf, "0x%lx:%lx 0x%lx:%lx",
			    &a.v[0], &a.v[1], &b.v[0], &b.v[1]) != 4) {
			printf("eh?\n");
			continue;
		}
		m.q = __muldi3(a.q, b.q);
		printf("%lx:%lx * %lx:%lx => %lx:%lx\n",
		    a.v[0], a.v[1], b.v[0], b.v[1], m.v[0], m.v[1]);
		printf("  = %lX%08lX * %lX%08lX => %lX%08lX\n",
		    a.v[0], a.v[1], b.v[0], b.v[1], m.v[0], m.v[1]);
	}
	exit(0);
}
