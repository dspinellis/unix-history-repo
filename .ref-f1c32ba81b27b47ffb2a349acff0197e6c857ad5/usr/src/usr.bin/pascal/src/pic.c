/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pic.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "OPnames.h"

main()  {
	register int j, k;

	for(j = 0;  j < 32;  j++) {
		for (k = 0;  k < 256;  k += 32)
			if (otext[j+k])
				printf("%03o%cO_%s\t", j+k, *otext[j+k], otext[j+k]+1);
			else
				printf("%03o\t\t", j+k);
		putchar('\n');
		if ((j+1)%8 == 0)
			putchar('\n');
	}
	printf("Starred opcodes are used internally in Pi and are never generated.\n");
	exit(0);
}
