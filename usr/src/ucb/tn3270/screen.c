/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */


/* this file implements primitives to drive the screen. */
#ifndef	lint
static	char	sccsid[] = "@(#)screen.c	2.1	4/11/85";
#endif	/* ndef lint */

#include <stdio.h>

#include "screen.h"
#include "3270.h"

char CIABuffer[64] = {
    0x40, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
    0x50, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
    0xd8, 0xd9, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
    0x60, 0x61, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
    0xe8, 0xe9, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
    0xf8, 0xf9, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
};

/* These are the routines compiled if we are using a parallel array to hold
	the field information...
 */

/* What is the screen address of the attribute byte for the terminal */

WhereTermAttrByte(p)
register int	p;
{
    register int i;

    i = p;

    do {
	if (TermIsStartField(i)) {
	    return(i);
	}
	i = ScreenDec(i);
    } while (i != p);

    return(LowestScreen());	/* unformatted screen... */
}

/* What we know is that table is of size SCREENSIZE */

FieldFind(table, position, failure)
register char *table;		/* which table of bytes to use */
register int position;		/* what position to start from */
int failure;			/* if unformatted, what value to return */
{
    register int ourp;

    ourp = position + 1 + bskip(table+position+1, SCREENSIZE-position-1, 0);
    if (ourp < SCREENSIZE) {
	return(ourp);
    }
    /* No fields in table after position.  Look for fields from beginning
     * of table.
     */
    ourp = bskip(table, position+1, 0);
    if (ourp <= position) {
	return(ourp);
    }
    return(failure);
}
