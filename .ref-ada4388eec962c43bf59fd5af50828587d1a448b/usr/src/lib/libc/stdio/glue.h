/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)glue.h	8.1 (Berkeley) %G%
 */

/*
 * The first few FILEs are statically allocated; others are dynamically
 * allocated and linked in via this glue structure.
 */
struct glue {
	struct	glue *next;
	int	niobs;
	FILE	*iobs;
} __sglue;
