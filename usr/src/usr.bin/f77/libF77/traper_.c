/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)traper_.c	5.1	%G%
 */
 * Full of Magic! DON'T CHANGE ANYTHING !!
 *
 * To use from f77:
 *	integer oldmsk, traper
 *	oldmsk = traper (mask)
 * where:
 *	mask = 1 to trap integer overflow
 *	mask = 2 to trap floating underflow
 *	mask = 3 to trap both
 *	These 2 bits will be set into the PSW.
 *	The old state will be returned.
 */

long traper_(msk)
long	*msk;
{
	int	old = 0;
#if	vax
#define IOV_MASK	0140
	int	**s = &msk;
	int	psw;

	s -= 5;
	psw = (int)*s;
	old = (psw & IOV_MASK) >> 5;
	psw = (psw & ~IOV_MASK) | ((*msk << 5) & IOV_MASK);
	*s = (int *)psw;
#endif	vax
	return((long)old);
}
