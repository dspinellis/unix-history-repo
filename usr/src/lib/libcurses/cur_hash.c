/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cur_hash.c	5.1 (Berkeley) %G%";
#endif	/* not lint */

/*
 * __hash() is "hashpjw" from the Dragon Book, Aho, Sethi & Ullman, p.436.
 */
u_int
__hash(s)
	char *s;
{
        register u_int	h, g;

	h = 0;
        while (*s) {
                h = (h << 4) + *s++;
                if (g = h & 0xf0000000) {
                        h = h ^ (g >> 24);
                        h = h ^ g;
                }
	}
        return h;
}
