/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)log.c	5.6 (Berkeley) 10/9/90";
#endif /* not lint */

/* LOG(X)
 * RETURN THE LOGARITHM OF x 
 * DOUBLE PRECISION (VAX D FORMAT 56 bits or IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/19/85;
 * REVISED BY K.C. NG on 2/7/85, 3/7/85, 3/24/85, 4/16/85.
 *
 * Required system supported functions:
 *	scalb(x,n)
 *	copysign(x,y)
 *	logb(x)	
 *	finite(x)
 *
 * Required kernel function:
 *	log__L(z) 
 *
 * Method :
 *	1. Argument Reduction: find k and f such that 
 *			x = 2^k * (1+f), 
 *	   where  sqrt(2)/2 < 1+f < sqrt(2) .
 *
 *	2. Let s = f/(2+f) ; based on log(1+f) = log(1+s) - log(1-s)
 *		 = 2s + 2/3 s**3 + 2/5 s**5 + .....,
 *	   log(1+f) is computed by
 *
 *	     		log(1+f) = 2s + s*log__L(s*s)
 *	   where
 *		log__L(z) = z*(L1 + z*(L2 + z*(... (L6 + z*L7)...)))
 *
 *	   See log__L() for the values of the coefficients.
 *
 *	3. Finally,  log(x) = k*ln2 + log(1+f).  (Here n*ln2 will be stored
 *	   in two floating point number: n*ln2hi + n*ln2lo, n*ln2hi is exact
 *	   since the last 20 bits of ln2hi is 0.)
 *
 * Special cases:
 *	log(x) is NaN with signal if x < 0 (including -INF) ; 
 *	log(+INF) is +INF; log(0) is -INF with signal;
 *	log(NaN) is that NaN with no signal.
 *
 * Accuracy:
 *	log(x) returns the exact log(x) nearly rounded. In a test run with
 *	1,536,000 random arguments on a VAX, the maximum observed error was
 *	.826 ulps (units in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#include <errno.h>
#include "mathimpl.h"

vc(ln2hi, 6.9314718055829871446E-1  ,7217,4031,0000,f7d0,   0, .B17217F7D00000)
vc(ln2lo, 1.6465949582897081279E-12 ,bcd5,2ce7,d9cc,e4f1, -39, .E7BCD5E4F1D9CC)
vc(sqrt2, 1.4142135623730950622E0   ,04f3,40b5,de65,33f9,   1, .B504F333F9DE65)

ic(ln2hi, 6.9314718036912381649E-1,   -1, 1.62E42FEE00000)
ic(ln2lo, 1.9082149292705877000E-10, -33, 1.A39EF35793C76)
ic(sqrt2, 1.4142135623730951455E0,     0, 1.6A09E667F3BCD)

#ifdef vccast
#define	ln2hi	vccast(ln2hi)
#define	ln2lo	vccast(ln2lo)
#define	sqrt2	vccast(sqrt2)
#endif


double log(x)
double x;
{
	const static double zero=0.0, negone= -1.0, half=1.0/2.0;
	double s,z,t;
	int k,n;

#if !defined(vax)&&!defined(tahoe)
	if(x!=x) return(x);	/* x is NaN */
#endif	/* !defined(vax)&&!defined(tahoe) */
	if(finite(x)) {
	   if( x > zero ) {

	   /* argument reduction */
	      k=logb(x);   x=scalb(x,-k);
	      if(k == -1022) /* subnormal no. */
		   {n=logb(x); x=scalb(x,-n); k+=n;} 
	      if(x >= sqrt2 ) {k += 1; x *= half;}
	      x += negone ;

	   /* compute log(1+x)  */
              s=x/(2+x); t=x*x*half;
	      z=k*ln2lo+s*(t+log__L(s*s));
	      x += (z - t) ;

	      return(k*ln2hi+x);
	   }
	/* end of if (x > zero) */

	   else {
#if defined(vax)||defined(tahoe)
		if ( x == zero )
		    return (infnan(-ERANGE));	/* -INF */
		else
		    return (infnan(EDOM));	/* NaN */
#else	/* defined(vax)||defined(tahoe) */
		/* zero argument, return -INF with signal */
		if ( x == zero )
		    return( negone/zero );

		/* negative argument, return NaN with signal */
		else 
		    return ( zero / zero );
#endif	/* defined(vax)||defined(tahoe) */
	    }
	}
    /* end of if (finite(x)) */
    /* NOTREACHED if defined(vax)||defined(tahoe) */

    /* log(-INF) is NaN with signal */
	else if (x<0) 
	    return(zero/zero);      

    /* log(+INF) is +INF */
	else return(x);      

}
