/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getbsize.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>

char *
getbsize(prog, headerlenp, blocksizep)
	char *prog;
	int *headerlenp;
	long *blocksizep;
{
	static char header[20];
	long n, max, mul, blocksize;
	char *ep, *p, *form;

#define	KB	(1024L)
#define	MB	(1024L * 1024L)
#define	GB	(1024L * 1024L * 1024L)
#define	MAXB	GB		/* No tera, peta, nor exa. */
	form = "";
	if ((p = getenv("BLOCKSIZE")) != NULL && *p != '\0') {
		if ((n = strtol(p, &ep, 10)) < 0)
			goto underflow;
		if (n == 0)
			n = 1;
		if (*ep && ep[1])
			goto fmterr;
		switch (*ep) {
		case 'G': case 'g':
			form = "G";
			max = MAXB / GB;
			mul = GB;
			break;
		case 'K': case 'k':
			form = "K";
			max = MAXB / KB;
			mul = KB;
			break;
		case 'M': case 'm':
			form = "M";
			max = MAXB / MB;
			mul = MB;
			break;
		case '\0':
			max = MAXB;
			mul = 1;
			break;
		default:
fmterr:			(void)fprintf(stderr,
			    "%s: %s: unknown blocksize\n", prog, p);
			n = 512;
			mul = 1;
			break;
		}
		if (n > max) {
			(void)fprintf(stderr,
			    "%s: maximum blocksize is %dG\n", prog, MAXB / GB);
			n = max;
		}
		if ((blocksize = n * mul) < 512) {
underflow:		(void)fprintf(stderr,
			    "%s: minimum blocksize is 512\n", prog);
			form = "";
			blocksize = n = 512;
		}
	} else
		blocksize = n = 512;

	(void)snprintf(header, sizeof(header), "%d%s-blocks", n, form);
	*headerlenp = strlen(header);
	*blocksizep = blocksize;
	return (header);
}
