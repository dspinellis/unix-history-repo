/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)gets.c	7.1 (Berkeley) %G%
 */

#include <pmax/stand/dec_prom.h>

char *
gets(s)
	char *s;
{
	return (callv->gets(s));
}
