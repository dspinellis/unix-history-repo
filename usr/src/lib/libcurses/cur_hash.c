/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cur_hash.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

#include <sys/types.h>


/*
 * __hash() is "hashpjw" from the Dragon Book, Aho, Sethi & Ullman, p.436.
 */
u_int
__hash(s, len)
	char *s;
	int len;
{
        register u_int	h, g, i;

	h = 0;
	i = 0;
        while (i < len) {
                h = (h << 4) + s[i];
                if (g = h & 0xf0000000) {
                        h = h ^ (g >> 24);
                        h = h ^ g;
                }
		i++;
	}
        return h;
}
