/*
 *	Copyright (c) 1984, 1985, 1986 by the Regents of the
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
static	char	sccsid[] = "@(#)globals.c	3.1  10/29/86";
#endif	/* ndef lint */

/*
 *	Do the defining instances for the globals of tn3270.
 */

#include	"../ctlr/hostctlr.h"
#include	"../ascii/ascebc.h"
#include	"../ctlr/dctype.h"
#include	"../ctlr/oia.h"
#include	"../ctlr/options.h"
#include	"../ctlr/screen.h"
#include	"../ascii/state.h"


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
