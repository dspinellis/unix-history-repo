/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mappings.h	5.3 (Berkeley) %G%
 */

/*
 * Mappings definitions.
 *
 * The mappings module is the interface between the object code and
 * source file representations of the program.
 *
 * This module is strongly tied to the object module, and needs the
 * most of the data defined in "object.h".
 */

ADDRESS objaddr();	/* get the object address corresponding to a line */

char *srcfilename();	/* get the nearest source file <= a given address */
LINENO srcline();	/* get the nearest source line <= a given address */
LINENO linelookup();	/* look for a line number with exactly given address */

int newfunc();		/* record the appearance of a new function */
SYM *whatblock();	/* find the function associated with an address */
int clrfunctab();	/* re-initialize function table */
