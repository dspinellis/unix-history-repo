/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)linetab.h	8.1 (Berkeley) %G%
 */

/*
 * definition of line number information table
 */

typedef struct {
	LINENO line;
	ADDRESS addr;
} LINETAB;

LINETAB *linetab;
