/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "aed.h"

/*
 * The following table is used to convert numbers to hex.  We cannot use
 * standard C library conversion because it generates lower case letters
 * which are bad news to the AED512.
 */

static char hex[] = "0123456789ABCDEF";

/*---------------------------------------------------------
 *	This is a local routine that converts an integer to a string
 *	of hexadecimal characters.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string contains the value of the low-order nchars 4-bit chunks
 *	of val, as represented in hexadecimal.  String is zero-filled.
 *---------------------------------------------------------
 */
chex(val, string, nchars)
int val;			/* Integer value to be converted. */
char *string;			/* Pointer to area for converted result. */
int nchars;			/* Number of characters to be converted. */
{
    string = &(string[nchars]);
    *string = '\0';
    for (; nchars>0 ; nchars--)
    {
	*(--string) = hex[val & 017];
	val >>= 4;
    }
}

/*---------------------------------------------------------
 *	This local routine outputs an x-y coordinate pair in the standard
 *	format required by the AED display.
 *
 *	Results:	None.
 *	
 *	Side Effects:
 *	Characters are output to the AED512 in the standard way required
 *	for values indicated by "xy20" in the user manual.
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
outxy20(x, y)
int x, y;			/* The coordinates to be output.  Note:
				 * these are world coordinates, not screen
				 * ones.  We scale in this routine.
				 */
{
    char s1[4], s2[4], s3[4];
    x = ((x - xbot) * scale)>>12;
    y = ((y - ybot) * scale)>>12;
    chex(((y>>8)&03) | ((x>>6)&014), s1, 1);
    chex(x&0377, s2, 2);
    chex(y&0377, s3, 2);
    fprintf(stdout, "%s%s%s", s1, s2, s3);
}

/*---------------------------------------------------------
 *	This routine sets the display's current color.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The current color in the display is set to pcolor, if it
 *	isn't that already.
 *---------------------------------------------------------
 */
setcolor(pcolor)
char *pcolor;			/* Pointer to a string giving the desired
				 * color in hexadecimal.
				 */
{
    static char curcolor[] = "xx";
    if ((pcolor[0] != curcolor[0]) || (pcolor[1] != curcolor[1]))
    {
	curcolor[0] = pcolor[0];
	curcolor[1] = pcolor[1];
	putc('L', stdout);
	fputs(curcolor, stdout);
    }
}
