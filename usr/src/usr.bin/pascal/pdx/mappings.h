/* Copyright (c) 1982 Regents of the University of California */

/* static char sccsid[] = "@(#)mappings.h 1.2 %G%"; */

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

newfunc();		/* record the appearance of a new function */
SYM *whatblock();	/* find the function associated with an address */
clrfunctab();		/* re-initialize function table */
