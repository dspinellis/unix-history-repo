#ifndef lint
static char *rcsid_keyboard_c = "$Header: keyboard.c,v 10.1 86/11/19 10:42:06 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* keyboard.c - key code processing routine
 *
 *	ProcessInput	matches RT/PC keyboard codes to
 *			to LK201 (DEC) codes.
 *
 *	Author:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *		Providence, RI 02912
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "keymatch.h"

ProcessInput (ev)
	register vsEvent *ev;
{

	if (ev->vse_device == VSE_DKB) {
		register int vse_key = ((int) ev->vse_key) & 0xFF;
		if ((vse_key >= 0) && (vse_key <= RTPC_CODES))
			ev->vse_key = keymatch[vse_key];
	}
	Deal_with_input (ev);
	return;
}
