/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)filetab.h	5.1 (Berkeley) %G%
 */

/*
 * definition of file table
 */

typedef struct {
	LINENO line;
	ADDRESS addr;
	char *filename;
	LINENO lineindex;
} FILETAB;

FILETAB *filetab;
