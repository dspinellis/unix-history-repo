/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pow.c	5.1 (Berkeley) 5/8/85";
#endif not lint

/*
	computes a^b.
	uses log and exp
*/

#include	<errno.h>
int errno;
double log(), exp();

double
pow(arg1,arg2)
double arg1, arg2;
{
	double temp;
	long l;

	asm("	bispsw	$0xe0");
	if(arg1 <= 0.) {
		if(arg1 == 0.) {
			if(arg2 <= 0.)
				goto domain;
			return(0.);
		}
		l = arg2;
		if(l != arg2)
			goto domain;
		temp = exp(arg2 * log(-arg1));
		if(l & 1)
			temp = -temp;
		return(temp);
	}
	return(exp(arg2 * log(arg1)));

domain:
	errno = EDOM;
	return(0.);
}
