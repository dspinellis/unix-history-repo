#include <X/mit-copyright.h>

/* $Header: XMenuDelSel.c,v 10.8 86/02/12 16:19:00 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuDeleteSelection - Deletes a selection from an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include "XMenuInternal.h"

int
XMenuDeleteSelection(menu, p_num, s_num)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be deleted. */
    register int s_num;		/* Selection number to be deleted. */
{
    register int i;		/* Loop index. */
    register XMPane *p_ptr;	/* Pointer to pane being deleted. */
    register XMSelect *s_ptr;	/* Pointer to selections being deleted. */
        
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
     * Remove the selection from the association table.
     */
    XDeleteAssoc(menu->assoc_tab, s_ptr->window);

    /*
     * Remove the selection from the parent pane's selection
     * list and update the selection count.
     */
    remque(s_ptr);
    p_ptr->s_count--;

    /*
     * Destroy the selection transparency.
     */
    if (s_ptr->window) XDestroyWindow(s_ptr->window);
    
    /*
     * Free the selection's XMSelect structure.
     */
    free(s_ptr);

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just deleted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}
