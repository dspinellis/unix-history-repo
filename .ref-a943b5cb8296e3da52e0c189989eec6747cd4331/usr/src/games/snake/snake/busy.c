/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)busy.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * busy: print an indication of how busy the system is for games.
 */
#ifndef MAX
# define MAX 30
#endif

#include <stdio.h>
main(argc, argv)
char **argv;
{
	double la[3];
	double max;

	loadav(la);
	max = la[0];
	if (la[1] > max) max = la[1];
	if (la[2] > max) max = la[2];
	if (argc > 1)
		printf("1=%g, 5=%g, 15=%g, max=%g\n", la[0], la[1], la[2], max);
	if (max > MAX)
		printf("100\n");	/* incredibly high, no games allowed */
	else
		printf("0\n");
	exit(0);
}

#include <sys/types.h>
#include <a.out.h>

struct	nlist nl[] = {
	{ "_avenrun" },
	{ 0 },
};

loadav(avenrun)
double	*avenrun;
{
	register int i;
	int	kmem;

	if ((kmem = open("/dev/kmem", 0)) < 0) {
		fprintf(stderr, "No kmem\n");
		exit(1);
	}
	nlist("/vmunix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}

	lseek(kmem, (long)nl[0].n_value, 0);
	read(kmem, avenrun, 3*sizeof(*avenrun));
}
