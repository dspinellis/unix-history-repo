/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)astosc.h	3.2 (Berkeley) %G%
 */

/*
 * This defines the structure used to translate:
 *
 *	ascii name ==> (scancode, shiftstate)
 *
 * (Actually, map3270 does "ascii name ==> index", and
 * termin does "index ==> (scancode, shiftstate)".  Both
 * mappings use this structure.)
 */

#define	INCLUDED_ASTOSC

struct astosc {
    unsigned char
	scancode,		/* Scan code for this function */
	shiftstate;		/* Shift state for this function */
    enum ctlrfcn function;	/* Internal function identifier */
    char *name;			/* Name of this function */
};

int ascii_to_index();		/* Function to feed InitControl() */

extern struct astosc astosc[256];
