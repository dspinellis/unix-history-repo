/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)acos.c	5.1 (Berkeley) %G%";
#endif not lint

/*
acos(arg) return the arccos,
	respectively of their arguments.

	Arctan is called after appropriate range reduction.
*/

#include	<errno.h>
int errno;
double atan();
double asin();
static double pio2	= 1.570796326794896619;

double
acos(arg) double arg; {

	asm("	bispsw	$0xe0");
	if(arg > 1.|| arg < -1.){
		errno = EDOM;
		return(0.);
	}

	return(pio2 - asin(arg));
}
