#include <X11/copyright.h>

/* $Header: SetAEQ.c,v 1.2 87/12/20 12:05:51 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetAEQ - Set Asynchronous event queueing mode.
 *		      When enabled asynchronous events will be queue while
 *		      a menu is being displayed and restored to the X
 *		      event queue when the menu is taken down.
 *
 *	Author:		Tony Della Fera, DEC
 *			March 12, 1986
 *
 */

#include "XMenuInt.h"

XMenuSetAEQ(menu, aeq)
    register XMenu *menu;	/* Menu object to be modified. */
    register int aeq;		/* AEQ mode? */
{
    /*
     * Set the AEQ mode switch.
     */
    menu->aeq = aeq;
}
