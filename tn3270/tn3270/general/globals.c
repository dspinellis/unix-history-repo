/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)globals.c	4.1 (Berkeley) 12/4/88";
#endif /* not lint */

/*
 *	Do the defining instances for the globals of tn3270.
 */

#include	"../ctlr/hostctlr.h"
#include	"../ctlr/oia.h"
#include	"../ctlr/options.h"
#include	"../ctlr/screen.h"


#define DEFINING_INSTANCES

#include	"globals.h"

#include	"../general/general.h"

/*
 * init_system()
 *
 * Initialize the global values in case of a restart.
 */

void
init_system()
{
    OptHome = OptLeftMargin = OptAPLmode = OptNullProcessing = 0;
    OptZonesMode = OptEnterNL = OptColFieldTab = OptPacing = 0;
    OptAlphaInNumeric = OptHome = OptLeftMargin = OptWordWrap = 0;

    ClearArray(Host);
    CursorAddress = BufferAddress = 0;

    Lowest = Highest = 0;

    UnLocked = AidByte = 0;

}
