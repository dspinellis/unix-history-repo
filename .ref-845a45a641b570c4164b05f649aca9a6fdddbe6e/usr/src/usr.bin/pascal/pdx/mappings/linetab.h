/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)linetab.h	5.2 (Berkeley) %G%
 */

/*
 * definition of line number information table
 */

typedef struct {
	LINENO line;
	ADDRESS addr;
} LINETAB;

LINETAB *linetab;
