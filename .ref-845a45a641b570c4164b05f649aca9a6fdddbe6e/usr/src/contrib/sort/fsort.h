/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Peter McIlroy.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fsort.h	5.1 (Berkeley) %G%
 */

#define POW 20			/* exponent for buffer size */
#define BUFSIZE (1 << POW)
#define MAXNUM (BUFSIZE/10)	/* lowish guess at average record size */
#define BUFFEND (EOF-2)
#define MAXFCT 1000
#define MAXLLEN ((1 << min(POW-4, 16)) - 14)

extern u_char **keylist, **l2buf, *buffer, *linebuf;

/* temp files in the stack have a file descriptor, a largest bin (maxb)
 * which becomes the last non-empty bin (lastb) when the actual largest
 * bin is smaller than max(half the total file, BUFSIZE)
 * Max_o is the offset of maxb so it can be sought after the other bins
 * are sorted.
*/
struct tempfile {
	FILE *fd;
	u_char maxb;
	u_char lastb;
	long max_o;
};
extern struct tempfile fstack[MAXFCT];
