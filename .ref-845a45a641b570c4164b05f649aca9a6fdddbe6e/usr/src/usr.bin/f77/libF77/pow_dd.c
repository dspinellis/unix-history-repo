/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pow_dd.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double pow_dd(ap, bp)
double *ap, *bp;
{
double pow();

return(pow(*ap, *bp) );
}
