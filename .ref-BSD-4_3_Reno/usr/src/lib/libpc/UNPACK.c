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
static char sccsid[] = "@(#)UNPACK.c	1.4 (Berkeley) 4/9/90";
#endif /* not lint */

/*
 * unpack(z,a,i)
 *
 * with:	z and a as in pack
 *
 * semantics:	for j := u to v do
 *			a[j-u+i] := z[j]
 */

UNPACK(i, a, z, size_a, lb_a, ub_a, size_z)

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
		ERROR("i = %D: Bad i to unpack(z,a,i)\n", i);
		return;
	}
	cp = &a[subscr * size_a];
	limit = cp + size_z;
	do	{
		*cp++ = *zp++;
	} while (cp < limit);
}
