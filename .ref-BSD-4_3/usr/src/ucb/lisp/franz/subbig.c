#ifndef lint
static char *rcsid =
   "$Header: subbig.c,v 1.2 83/09/12 14:17:31 sklower Exp $";
#endif

/*					-[Sat Jan 29 13:36:05 1983 by jkf]-
 * 	subbig.c			$Locker:  $
 * bignum subtraction
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"

/*
 * subbig -- subtract one bignum from another.
 *
 * What this does is it negates each coefficient of a copy of the bignum
 * which is just pushed on the stack for convenience.  This may give rise
 * to a bignum which is not in canonical form, but is nonetheless a repre
 * sentation of a bignum.  Addbig then adds it to a bignum, and produces
 * a result in canonical form.
 */
lispval
subbig(pos,neg)
lispval pos, neg;
{
	register lispval work;
	lispval adbig();
	register long *mysp = sp() - 2;
	register long *ersatz = mysp;
	Keepxs();

	for(work = neg; work!=0; work = work->s.CDR) {
		stack((long)(mysp -= 2));
		stack(-work->i);
	}
	mysp[3] = 0;
	work = (adbig(pos,(lispval)ersatz));
	Freexs();
	return(work);
}
