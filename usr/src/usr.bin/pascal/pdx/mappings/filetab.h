/* Copyright (c) 1982 Regents of the University of California */

/* static char sccsid[] = "@(#)filetab.h 1.2 %G%"; */

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
