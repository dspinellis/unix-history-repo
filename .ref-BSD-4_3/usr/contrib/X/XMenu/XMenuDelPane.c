#include <X/mit-copyright.h>

/* $Header: XMenuDelPane.c,v 10.8 86/02/01 16:14:44 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuDeletePane - Deletes a pane from an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include "XMenuInternal.h"

int
XMenuDeletePane(menu, p_num)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be deleted. */
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
     * Remove the pane from the association table.
     */
    XDeleteAssoc(menu->assoc_tab, p_ptr->window);

    /*
     * Remove the pane from the pane list and update
     * the pane count.
     */
    remque(p_ptr);
    menu->p_count--;

    /*
     * Remove all the selections in the pane from the
     * association table and free their XMSelect structures.
     */
    s_ptr = p_ptr->s_list;
    for (i = 0; i < p_ptr->s_count; i++) {
	XDeleteAssoc(menu->assoc_tab, s_ptr->prev->window);
	free(s_ptr->prev);
	s_ptr = s_ptr->next;
    }
    free(s_ptr);

    if (p_ptr->window) {
	/*
	 * Destroy the selection transparencies.
	 */
	XDestroySubwindows(p_ptr->window);
    
	/*
	 * Destroy the pane window.
	 */
	XDestroyWindow(p_ptr->window);
    }
    
    /*
     * Free the pane's XMPane structure.
     */
    free(p_ptr);

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just deleted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}
