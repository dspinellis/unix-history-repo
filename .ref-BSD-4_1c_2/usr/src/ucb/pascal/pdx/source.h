/* Copyright (c) 1982 Regents of the University of California */

/* static char sccsid[] = "@(#)source.h 1.2 1/18/82"; */

/*
 * Definitions for interfacing with source management routines.
 */

char *cursource;		/* current source file name */
LINENO lastlinenum;		/* last source line number */

skimsource();			/* get seek pointers to source lines */
chkline();			/* checks to see that a line number is valid */
printlines();			/* print out from first line to second */
