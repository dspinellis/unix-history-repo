/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PACK.c 1.1 10/29/80";

#include "h01errs.h"

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
		*zp++ = *cp++;
	} while (cp < limit);
}
