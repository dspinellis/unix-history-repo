/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)loc_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Return the address of the argument.
 *
 * calling sequence:
 *	iloc = loc (arg)
 * where:
 *	iloc will receive the address of arg
 */

long loc_(arg)
long *arg;
{
	return((long)arg);
}
