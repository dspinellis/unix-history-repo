/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)traper_.c	5.3	11/3/86
 *
 * Full of Magic! DON'T CHANGE ANYTHING !!
 *
 * To use from f77:
 *	integer oldmsk, traper
 *	oldmsk = traper (mask)
 * where for vax:
 *	mask = 1 to trap integer overflow
 *	mask = 2 to trap floating underflow
 *	mask = 3 to trap both
 *	These 2 bits will be set into the PSW.
 *	The old state will be returned.
 *
 * where for CCI:
 *	mask = 0 to trap neither
 *	mask = 1 to trap integer overflow
 *	mask = 2 to trap floating underflow
 *	mask = 3 to trap both
 *	These 2 bits will be set into the PSL.
 *	The old state will be returned.
 */

#ifdef vax
long traper_(msk)
long	*msk;
{
	int	old = 0;
#define IOV_MASK	0140
	int	**s = &msk;
	int	psw;

	s -= 5;
	psw = (int)*s;
	old = (psw & IOV_MASK) >> 5;
	psw = (psw & ~IOV_MASK) | ((*msk << 5) & IOV_MASK);
	*s = (int *)psw;
	return((long)old);
}
#endif	vax

/*
 * Assumptions for CCI:
 *	- the two bits are contiguous in PSL;
 *	- integer overflow trap enable bit < floating underflow trap enable bit;
 */
#ifdef tahoe
# include <machine/psl.h>

unsigned long old_msk;
unsigned short new_msk;
unsigned long tst_msk;

long traper_(msk)
long	*msk;
{
#define IOV_MASK (PSL_IV | PSL_FU)
#define IOV_DISP 5

	asm("	movpsl _old_msk");

	old_msk = (old_msk & IOV_MASK) >> IOV_DISP;

	new_msk = (*msk << IOV_DISP) & IOV_MASK;
	asm("	bispsw _new_msk");

	new_msk = ~(*msk << IOV_DISP) & IOV_MASK;
	asm("	bicpsw _new_msk");

	return(old_msk);
}
#endif tahoe
