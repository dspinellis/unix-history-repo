/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)UNPACK.c	1.4 (Berkeley) %G%";
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
