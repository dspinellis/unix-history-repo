/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)linetab.h	5.1 (Berkeley) 6/6/85
 */

/*
 * definition of line number information table
 */

typedef struct {
	LINENO line;
	ADDRESS addr;
} LINETAB;

LINETAB *linetab;
