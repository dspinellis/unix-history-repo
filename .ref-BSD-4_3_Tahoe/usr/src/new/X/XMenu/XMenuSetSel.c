#include <X/mit-copyright.h>

/* $Header: XMenuSetSel.c,v 10.6 86/02/12 16:20:04 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetSelection - Set a menu selection to be active or inactive.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInternal.h"

int
XMenuSetSelection(menu, p_num, s_num, active)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    register int s_num;		/* Selection number to modified. */
    int active;			/* Make selection active? */
{
    register int i;		/* Loop counter. */
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */
    
    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the right selection.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, s_num);
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Set its active switch.
     */
    s_ptr->active = active;

    /*
     * Return the selection number just set.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}
