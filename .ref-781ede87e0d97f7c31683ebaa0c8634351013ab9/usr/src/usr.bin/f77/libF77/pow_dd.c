/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_dd.c	5.1	%G%
 */

double pow_dd(ap, bp)
double *ap, *bp;
{
double pow();

return(pow(*ap, *bp) );
}
