#include "global.h"

lispval
subbig(pos,neg)
lispval pos, neg;
{
	register lispval work;
	int *sp(); lispval adbig();
	register int *mysp = sp() - 2;
	register int *ersatz = mysp;

	for(work = neg; work!=0; work = work->CDR) {
		stack(-work->i, (mysp -= 2));
	}
	mysp[3] = 0;
	return(adbig(pos,ersatz));
}
/*
 * subbig -- subtract one bignum from another.
 *
 * What this does is it negates each coefficient of a copy of the bignum
 * which is just pushed on the stack for convenience.  This may give rise
 * to a bignum which is not in canonical form, but is nonetheless a repre
 * sentation of a bignum.  Addbig then adds it to a bignum, and produces
 * a result in canonical form.
 */
