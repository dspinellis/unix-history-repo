/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)PACK.c	1.4 (Berkeley) 4/9/90";
#endif /* not lint */

/*
 * pack(a,i,z)
 *
 * with:	a: array[m..n] of t
 *	z: packed array[u..v] of t
 *
 * semantics:	for j := u to v do
 *			z[j] := a[j-u+i];
 *
 * need to check:
 *	1. i >= m
 *	2. i+(v-u) <= n		(i.e. i-m <= (n-m)-(v-u))
 *
 * on stack:	lv(z), lv(a), rv(i) (len 4)
 *
 * move w(t)*(v-u+1) bytes from lv(a)+w(t)*(i-m) to lv(z)
 */

PACK(i, a, z, size_a, lb_a, ub_a, size_z)

	long	i;	/* subscript into a to begin packing */
	char	*a;	/* pointer to structure a */
	char	*z;	/* pointer to structure z */
	long	size_a;	/* sizeof(a_type) */
	long	lb_a;	/* lower bound of structure a */
	long	ub_a;	/* (upper bound of a) - (lb_a + sizeof(z_type)) */
	long	size_z;	/* sizeof(z_type) */
{
	int		subscr;
	register char	*cp;
	register char	*zp = z;
	register char	*limit;

	subscr = i - lb_a;
	if (subscr < 0 || subscr > ub_a) {
		ERROR("i = %D: Bad i to pack(a,i,z)\n", i);
		return;
	}
	cp = &a[subscr * size_a];
	limit = cp + size_z;
	do	{
		*zp++ = *cp++;
	} while (cp < limit);
}
