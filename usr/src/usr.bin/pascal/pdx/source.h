/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)source.h	5.1 (Berkeley) %G%
 */

*
 * Definitions for interfacing with source management routines.
 */

char *cursource;		/* current source file name */
LINENO lastlinenum;		/* last source line number */

skimsource();			/* get seek pointers to source lines */
chkline();			/* checks to see that a line number is valid */
printlines();			/* print out from first line to second */
