/*
 *	Copyright (c) 1984-1987 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
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

#ifndef	lint
static char sccsid[] = "@(#)options.c	3.1 (Berkeley) %G%";
#endif	/* ndef lint */

/*
 * this file contains the definitions, initialization, and processing of
 *	commands to handle the various local options (APL ON, etc.)
 */

#include "options.h"

#include "../general/globals.h"
#include "options.ext"

void
OptInit()
{
    register int i;

    OptAPLmode = 0;
    OptNullProcessing = 1;		/* improved null processing */
    OptZonesMode = 0;		/* zones mode off */
    OptEnterNL = 0;		/* regular enter/new line keys */
    OptColFieldTab = 0;		/* regular column/field tab keys */
    OptPacing = 1;			/* do pacing */
    OptAlphaInNumeric = 0;		/* allow alpha in numeric fields */
    for (i = 0; i < sizeof OptColTabs; i++) {
	OptColTabs[i] = ((i%8) == 0);	/* every 8 columns */
    }
    OptHome = 0;
    OptLeftMargin = 0;
    OptWordWrap = 0;
}

OptOrder(pointer, count, control)
char *pointer;
int count;
int control;
{
    int i, j, character, origCount;

    origCount = count;

    if (count == 0) {
	return(0);
    }
    character = *pointer&0xff;
    pointer++;
    count--;
    switch (character) {
    case 0xa0:
	OptAPLmode = 1;
	break;
    case 0x61:
	OptAPLmode = 0;
	break;
    case 0x95:
	OptNullProcessing = 0;
	break;
    case 0xd5:
	OptNullProcessing = 1;
	break;
    case 0xa9:
	OptZonesMode = 1;
	break;
    case 0xe9:
	OptZonesMode = 0;
	break;
    case 0x85:
	OptEnterNL = 1;
	break;
    case 0xc5:
	OptEnterNL = 0;
	break;
    case 0x83:
	OptColFieldTab = 1;
	break;
    case 0xc3:
	OptColFieldTab = 0;
	break;
    case 0x97:
	OptPacing = 0;
	break;
    case 0xd7:
	OptPacing = 1;
	break;
    case 0xa5:
	OptAlphaInNumeric = 1;
	break;
    case 0xe5:
	OptAlphaInNumeric = 0;
	break;
    case 0xe3:
	if (!control && count < 30) {
	    return(0);		/* want more! */
	}
	for (i = 0; i < sizeof OptColTabs; i++) {
	    OptColTabs[i] = 0;
	}
	if (!count) {
	    break;
	}
	j = (*pointer&0xff)-0x40;
	count--;
	pointer++;
	if (j < 0 || j >= 24) {
	    break;
	}
	OptHome = j;
	if (!count) {
	    break;
	}
	j = (*pointer&0xff)-0x40;
	count--;
	pointer++;
	if (j < 0 || j >= 80) {
	    break;
	}
	OptLeftMargin = j;
	if (!count) {
	    break;
	}
	i = count;
	if (i > 28) {
	    i = 28;
	}
	while (i) {
	    j = (*pointer&0xff)-0x40;
	    if (j < 0 || j >= sizeof OptColTabs) {
		break;
	    }
	    OptColTabs[j] = 1;
	    i --;
	    pointer++;
	    count--;
	}
	break;
    case 0xa6:
	OptWordWrap = 1;
	break;
    case 0xe6:
	OptWordWrap = 0;
	break;
    default:
	break;
    }
    return(origCount - count);
}
