/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ef1asc_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/* EFL support routine to copy string b to string a */

#define M	( (long) (sizeof(long) - 1) )
#define EVEN(x)	( ( (x)+ M) & (~M) )

ef1asc_(a, la, b, lb)
int *a, *b;
long int *la, *lb;
{
s_copy( (char *)a, (char *)b, EVEN(*la), *lb );
}
