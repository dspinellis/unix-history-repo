/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)filetab.h	5.2 (Berkeley) %G%
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
