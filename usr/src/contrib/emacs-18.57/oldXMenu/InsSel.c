#include <X11/copyright.h>

/* $Header: InsSel.c,v 1.3 87/12/20 12:05:16 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuInsertSelection - Inserts a selection into an XMenu object
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include "XMenuInt.h"

int
XMenuInsertSelection(menu, p_num, s_num, data, label, active)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    register int s_num;		/* Selection number of new selection. */
    char *data;			/* Data value. */
    char *label;		/* Selection label. */
    int active;			/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

    XMSelect *select;		/* Newly created selection. */

    int label_length;		/* Label lenght in characters. */
    int label_width;		/* Label width in pixels. */
    
    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the selection number one less than the one specified since that
     * is the selection after which the insertion will occur.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, (s_num - 1));
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Calloc the XMSelect structure.
     */
    select = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (select == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }

    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->s_fnt_info, label, label_length);


    /*
     * Fill the XMSelect structure.
     */
    select->type = SELECTION;
    select->active = active;
    select->serial = -1;
    select->label = label;
    select->label_width = label_width;
    select->label_length = label_length;
    select->data = data;
    select->parent_p = p_ptr;

    /*
     * Insert the selection after the selection with the selection
     * number one less than the desired number for the new selection.
     */
    insque(select, s_ptr);

    /*
     * Update the selection count.
     */
    p_ptr->s_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just inserted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}
