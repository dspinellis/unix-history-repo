/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)serrno_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

extern int errno;
serrno_(n)
long *n;
{	errno = *n; }
