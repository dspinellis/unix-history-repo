/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 */

#ifndef lint
static char sccsid[] = "@(#)log10.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/* LOG10(X)
 * RETURN THE BASE 10 LOGARITHM OF x
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/20/85; 
 * REVISED BY K.C. NG on 1/23/85, 3/7/85, 4/16/85.
 * 
 * Required kernel function:
 *	log(x)
 *
 * Method :
 *			     log(x)
 *		log10(x) = ---------  or  [1/log(10)]*log(x)
 *			    log(10)
 *
 *    Note:
 *	  [log(10)]   rounded to 56 bits has error  .0895  ulps,
 *	  [1/log(10)] rounded to 53 bits has error  .198   ulps;
 *	  therefore, for better accuracy, in VAX D format, we divide 
 *	  log(x) by log(10), but in IEEE Double format, we multiply 
 *	  log(x) by [1/log(10)].
 *
 * Special cases:
 *	log10(x) is NaN with signal if x < 0; 
 *	log10(+INF) is +INF with no signal; log10(0) is -INF with signal;
 *	log10(NaN) is that NaN with no signal.
 *
 * Accuracy:
 *	log10(X) returns the exact log10(x) nearly rounded. In a test run
 *	with 1,536,000 random arguments on a VAX, the maximum observed
 *	error was 1.74 ulps (units in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#include "mathimpl.h"

vc(ln10hi, 2.3025850929940456790E0 ,5d8d,4113,a8ac,ddaa, 2, .935D8DDDAAA8AC)

ic(ivln10, 4.3429448190325181667E-1, -2, 1.BCB7B1526E50E)

#ifdef vccast
#define	ln10hi	vccast(ln10hi)
#endif


double log10(x)
double x;
{
#if defined(vax)||defined(tahoe)
	return(log(x)/ln10hi);
#else	/* defined(vax)||defined(tahoe) */
	return(ivln10*log(x));
#endif	/* defined(vax)||defined(tahoe) */
}
