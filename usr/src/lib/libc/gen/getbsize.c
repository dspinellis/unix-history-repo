/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getbsize.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>

char *
getbsize(prog, headerlenp, blocksizep)
	char *prog;
	int *headerlenp, *blocksizep;
{
	static char header[20];
	long blocksize;
	char *ep, *p;

#define	Kb	1024
#define	Mb	1048576
#define	Gb	1073741824
	if ((p = getenv("BLOCKSIZE")) != NULL && *p != '\0') {
		blocksize = strtol(p, &ep, 10);
		switch(*ep) {
		case 'G': case 'g':
			if (ep[1])
				goto fmterr;
			if (blocksize > 1)
				goto overflow;
			*headerlenp = snprintf(header, sizeof(header),
			    "%dG-blocks", blocksize);
			*blocksizep = blocksize * Gb;
			return (header);
		case 'M': case 'm':
			if (ep[1])
				goto fmterr;
			*headerlenp = snprintf(header, sizeof(header),
			    "%dM-blocks", blocksize);
			*blocksizep = blocksize * Mb;
			return (header);
		case 'K': case 'k':
			if (ep[1])
				goto fmterr;
			*headerlenp = snprintf(header, sizeof(header),
			    "%dK-blocks", blocksize);
			*blocksizep = blocksize * Kb;
			return (header);
		case '\0':
			if (blocksize > Gb) {
overflow:			(void)fprintf(stderr,
				    "%s: maximum blocksize is 1G\n", prog);
				blocksize = 512;
			} else if (blocksize < 512) {
				(void)fprintf(stderr,
				    "%s: minimum blocksize is 512\n", prog);
				blocksize = 512;
			}
			break;
		default:
fmterr:			(void)fprintf(stderr,
			    "%s: %s: unknown blocksize\n", prog, p);
			blocksize = 512;
			break;
		}
	} else
		blocksize = 512;

	*headerlenp = snprintf(header, sizeof(header), "%d-blocks", blocksize);
	*blocksizep = blocksize;
	return (header);
}
