#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/subbig.c,v 1.1 83/01/29 13:37:56 jkf Exp $";
#endif

/*					-[Sat Jan 29 13:36:05 1983 by jkf]-
 * 	subbig.c			$Locker:  $
 * bignum subtraction
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#define SP() ((int *)sp())

lispval
subbig(pos,neg)
lispval pos, neg;
{
	register lispval work;
	char *sp(); lispval adbig();
	register int *mysp = SP() - 2;
	register int *ersatz = mysp;

	for(work = neg; work!=0; work = work->s.CDR) {
		stack((mysp -= 2));
		stack(-work->i);
	}
	mysp[3] = 0;
	return(adbig(pos,(lispval)ersatz));
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
