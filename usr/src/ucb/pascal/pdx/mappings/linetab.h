/* Copyright (c) 1982 Regents of the University of California */

/* static char sccsid[] = "@(#)linetab.h 1.2 1/18/82"; */

/*
 * definition of line number information table
 */

typedef struct {
	LINENO line;
	ADDRESS addr;
} LINETAB;

LINETAB *linetab;
