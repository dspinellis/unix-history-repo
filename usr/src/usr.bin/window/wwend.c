/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)wwend.c	3.10 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwend()
{
	wwupdate();
	if (tt.tt_scroll_top != 0 || tt.tt_scroll_bot != tt.tt_nrow - 1)
		/* tt.tt_setscroll is known to be defined */
		(*tt.tt_setscroll)(0, tt.tt_nrow - 1);
	if (tt.tt_insert)
		(*tt.tt_setinsert)(0);
	if (tt.tt_modes)
		(*tt.tt_setmodes)(0);
	if (tt.tt_scroll_down)
		(*tt.tt_scroll_down)();
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_end)();
	ttflush();
	(void) wwsettty(0, &wwoldtty, &wwnewtty);
}
