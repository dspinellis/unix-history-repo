/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)source.h	5.3 (Berkeley) 1/3/88
 */

/*
 * Definitions for interfacing with source management routines.
 */

char *cursource;		/* current source file name */
LINENO lastlinenum;		/* last source line number */

int skimsource();		/* get seek pointers to source lines */
int chkline();			/* checks to see that a line number is valid */
int printlines();		/* print out from first line to second */
