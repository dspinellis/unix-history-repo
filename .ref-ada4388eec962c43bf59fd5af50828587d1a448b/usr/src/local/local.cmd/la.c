/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)la.c	6.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * la - print load averages
 */
#include <stdio.h>

double	avenrun[3];

main(argc,argv)
	int argc;
	char *argv[];
{
	register int kmem, mem;

	if (getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0])) < 0) {
		fprintf(stderr, "%s: getloadavg: failed\n", argv[0]);
		exit(1);
	}

	if ((argc > 1) && (! strcmp (argv[1],"-g"))) {
		printf("1    5    15\n");
		printf("Min  Min  Min\n");
		printf("-------------\n");
	}
	printf("%.2f %.2f %.2f\n", avenrun[0], avenrun[1], avenrun[2]);
}
