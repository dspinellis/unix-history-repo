/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ef1cmc_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/* EFL support routine to compare two character strings */

long int ef1cmc_(a, la, b, lb)
int *a, *b;
long int *la, *lb;
{
return( s_cmp( (char *)a, (char *)b, *la, *lb) );
}
