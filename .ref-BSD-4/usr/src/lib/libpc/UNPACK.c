/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNPACK.c 1.1 10/29/80";

#include "h01errs.h"

/*
 * unpack(z,a,i)
 *
 * with:	z and a as in pack
 *
 * semantics:	for j := u to v do
 *			a[j-u+i] := z[j]
 */

UNPACK(i, a, z, size_a, lb_a, ub_a, size_z)

	int	i;	/* subscript into a to begin packing */
	char	*a;	/* pointer to structure a */
	char	*z;	/* pointer to structure z */
	int	size_a;	/* sizeof(a_type) */
	int	lb_a;	/* lower bound of structure a */
	int	ub_a;	/* (upper bound of a) - (lb_a + sizeof(z_type)) */
	int	size_z;	/* sizeof(z_type) */
{
	int		subscr;
	register char	*cp;
	register char	*zp = z;
	register char	*limit;

	subscr = i - lb_a;
	if (subscr < 0 || subscr > ub_a) {
		ERROR(EPACK, i);
		return;
	}
	cp = &a[subscr * size_a];
	limit = cp + size_z;
	do	{
		*cp++ = *zp++;
	} while (cp < limit);
}
