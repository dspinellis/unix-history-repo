#include <X/mit-copyright.h>

/* $Header: XMenuSetPane.c,v 10.6 86/02/12 16:19:59 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetPane - Set a menu pane to be active or inactive.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInternal.h"

int
XMenuSetPane(menu, p_num, active)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    register int active;	/* Make selection active? */
{
    register int i;		/* Loop counter. */
    register XMPane *p_ptr;	/* XMPane pointer. */

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Set its active switch.
     */
    p_ptr->active = active;

    /*
     * Return the pane number just set.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}
