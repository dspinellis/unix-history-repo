/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)l_gt.c	5.2 (Berkeley) %G%";
#endif /* not lint */

long int l_gt(a,b,la,lb)
char *a, *b;
long int la, lb;
{
return(s_cmp(a,b,la,lb) > 0);
}
