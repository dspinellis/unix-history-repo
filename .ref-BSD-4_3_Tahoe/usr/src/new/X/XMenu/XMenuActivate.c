/* $Header: XMenuActivate.c,v 10.19 86/07/11 16:50:09 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuActivate -	Maps a given menu to the display and activates
 *			the menu for user selection.  The user is allowed to
 *			specify which pane and selection will be current,
 *			the X and Y location of the menu (relative to the
 *			parent window) and the mouse button event mask that
 *			will be used to identify a selection request.
 *
 *			A menu selection is shown to be current by placing
 *			a highlight box around the selection as the mouse
 *			cursor enters its active region.  Inactive selections
 *			will not be highlited.	As the mouse cursor moved
 *			from one menu pane to another menu pane the pane being
 *			entered is raised and made current and the pane being
 *			left is lowered.
 *
 *			Anytime XMenuActivate returns, the p_num and
 *			s_num are left at their last known values (i.e.,
 *			the last known current pane and selection indices).
 *			The following are the defined return states:
 *
 *			1)	If at any time an error occurs the data
 *				pointer is left untouched and XM_FAILURE
 *				is returned.  
 *
 *			2)	When a selection request is recieved (i.e.,
 *				when the specified mouse event occurs) the
 *				data pointer will be set to the data
 *				associated with the particular selection
 *				current at the time of the selection request
 *				and XM_SUCCESS is returned.
 *
 *			3)	If no selection was current at the time a
 *				selection request is made the data pointer
 *				will be left untouched and XM_NO_SELECT will
 *				be returned.
 *
 *			4)	If the selection that was current at the time 
 *				a selection request is made is not an active
 *				selection the data pointer will be left
 *				untouched and XM_IA_SELECT will be returned.
 *
 *			Since X processes events in an asynchronous manner
 *			it is likely that XMenuActivate will encounter
 *			a "foreign event" while it is executing.  Foreign
 *			events are handled in one of three ways:
 *
 *			1)	The event is discarded.  This is the default
 *				mode and requires no action on the part of the
 *				application.
 *
 *			2)	The application has identified an asynchronous
 *				event handler that will be called and the
 *				foreign event handed off to it.  Note:
 *				AEQ mode disables this mode temporarily.
 *
 *			3)	The application has enabled asynchronous event
 *				queueing mode.  In this mode all foreign events
 *				will be	queued up untill XMenuActivate
 *				terminates; at which time they will be
 *				returned to the	X event queue.  As long as
 *				AEQ mode is enabled any asynchronous event
 *				handler as temporarily disabled.
 *
 *			Any events encountered while taking down the menu
 *			(i.e., exposure events from occluded windows) will
 *			automatically be returned to the X event queue after
 *			XMenuActivate has cleaned the queue of any of its own
 *			events that are no longer needed.
 *
 *	Author:		Tony Della Fera, DEC
 *			March 12, 1986
 *
 */

#include "XMenuInternal.h"

int
XMenuActivate(menu, p_num, s_num, x_pos, y_pos, event_mask, data)
    register XMenu *menu;		/* Menu to activate. */
    int *p_num;				/* Pane number selected. */
    int *s_num;				/* Selection number selected. */
    int x_pos;				/* X coordinate of menu position. */
    int y_pos;				/* Y coordinate of menu position. */
    int event_mask;			/* Mouse button event mask. */
    char **data;			/* Pointer to return data value. */
{
    register int i;			/* Loop counter. */
    int status;				/* X routine call status. */
    int orig_x;				/* Upper left menu origin X coord. */
    int orig_y;				/* Upper left menu origin Y coord. */
    int save_x;				/* Upper left X of save region. */
    int save_y;				/* Upper left Y of save region. */
    int save_w;				/* Width of pixmap save region. */
    int save_h;				/* Height of pixmap save region. */
    int save_w_offscr;			/* Pixmap save width off screen. */
    int save_h_offscr;			/* Pixmap save height off screen. */
    int x, y;				/* Dummy X and Y arguments. */
    int ret_val;			/* Return value. */

    register XMPane *p_ptr;		/* Current XMPane. */
    register XMPane *event_xmp;		/* Event XMPane pointer. */
    register XMPane *cur_p;		/* Current pane. */
    register XMSelect *cur_s;		/* Current selection. */
    XMWindow *event_xmw;		/* Event XMWindow pointer. */
    XEvent event;			/* X input event. */
    XCrossingEvent *xc_event;		/* X window crossing event. */
    Window xc_window;			/* X window crossing event window. */

    Pixmap save_pixmap;			/* Pixmap to save bits under menu. */

    Bool saved = TRUE;			/* Pixmap save succeeded. */
    Bool selection = FALSE;		/* Selection has been made. */
    Bool forward = TRUE;		/* Moving forward in the pane list. */
    Bool p_lock = TRUE;			/* Pane entrance lock. */
    Bool s_lock = TRUE;			/* Selection entrance lock. */

    /*
     * Define and allocate a foreign event queue to hold events
     * that don't belong to XMenu.  These events are later restored
     * to the X event queue.
     */
    typedef struct _xmeventque {
	XEvent event;
	struct _xmeventque *next;
    } XMEventQue;

    XMEventQue *feq = NULL;    		/* Foreign event queue. */
    XMEventQue *feq_tmp;		/* Foreign event queue temporary. */

    /*
     * Are the position arguments are positive?
     */
    if ((x_pos <= 0) || (y_pos <= 0)) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * If there are no panes in the menu then return failure
     * beacuse the menu is not initialized.
     */
    if (menu->p_count == 0) {
	_XMErrorCode = XME_NOT_INIT;
	return(XM_FAILURE);
    }

    /*
     * Find the desired current pane.
     */
    cur_p = _XMGetPanePtr(menu, *p_num);
    if (cur_p == NULL) return(XM_FAILURE);

    /*
     * Find the desired current selection.
     * If the current selection index is out of range a null current selection
     * will be assumed and the cursor will be placed in the current pane
     * header.
     */
    cur_s = _XMGetSelectionPtr(cur_p, *s_num);

    /*
     * Check to see that the menu's dependencies have been
     * recomputed and are up to date.  If not, do it now.
     */
    if (menu->recompute) XMenuRecompute(menu);

    /*
     * If the current pane is active then activate it.
     */
    if (cur_p->active) {
	cur_p->activated = 1;
	XChangeBackground(cur_p->window, menu->bkgnd_pixmap);
    }

    /*
     * Compute the new menu origin such that the cursor hot point lies
     * in the center of the desired current pane and selection.
     */
    _XMTransToOrigin(menu, cur_p, cur_s, x_pos, y_pos, &orig_x, &orig_y);

    /*
     * Then move all the panes into position relative to the newly
     * computed origin.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	XMoveWindow(
	    p_ptr->window,
	    orig_x + p_ptr->window_x,
	    orig_y + p_ptr->window_y
	);
    }

    /*<
     * If server freeze mode is selected...
     */
    if (menu->freeze) {
	/*
	 * Compute pixmap save region.
	 */
	save_x = max(orig_x, 0);
	save_y = max(orig_y, 0);
	save_w_offscr = (orig_x + menu->width) - DisplayWidth();
	save_h_offscr = (orig_y + menu->height) - DisplayHeight();
	if (save_w_offscr < 0) save_w = menu->width;
	else save_w = menu->width - save_w_offscr;
	if (save_h_offscr < 0) save_h = menu->height;
	else save_h = menu->height - save_h_offscr;

	/*
	 * Grab the X server.
	 */
	XGrabServer();

	/* 
	 * Save the bits under where the menu will be.
	 */
	save_pixmap = XPixmapSave(
	    menu->parent,
	    save_x, save_y,
	    save_w, save_h
	);
	if (save_pixmap == _X_FAILURE) saved = FALSE;
    }
    else {
	saved = FALSE;
    }

    /*
     * Synchronize the X buffers and the event queue.
     * From here on, all events in the queue that don't belong to
     * XMenu are send back to the application via an application
     * provided event handler or discarded if the application has
     * not provided an event handler.
     */
    XSync(0);
    
    /*
     * Grab the mouse for menu input.
     */
    status = XGrabMouse(menu->parent, menu->mouse_cursor, event_mask);
    if (status == _X_FAILURE) {
	_XMErrorCode = XME_GRAB_MOUSE;
	return(XM_FAILURE);
    }

    /*
     * Map the menu panes.
     */
    for (
	p_ptr = menu->p_list->prev;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->prev
    ){
	if (p_ptr == cur_p) break;
	XMapWindow(p_ptr->window);
    }
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	if (p_ptr == cur_p) break;
	XMapWindow(p_ptr->window);
    }
    XMapWindow(cur_p->window);
    
    /*
     * Clear the current selection.
     */
    cur_s = NULL;

    /*
     * Begin event processing loop.
     */
    while (1) {
	/*
	 * Fetch the next event.
	 */
	XNextEvent(&event);
	/*
	 * Dispatch on the event type.
	 */
	switch (event.type) {
	    case ExposeWindow:
		event_xmp = (XMPane *)XLookUpAssoc(
		    menu->assoc_tab, event.window
		);
		if (event_xmp == NULL) {
		    /*
		     * If AEQ mode is enabled then queue the event.
		     */
		    if (menu->aeq) {
			feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
			if (feq_tmp == NULL) {
			    _XMErrorCode = XME_CALLOC;
			    return(XM_FAILURE);
			}
			feq_tmp->event = event;
			feq_tmp->next = feq;
			feq = feq_tmp;
		    }
		    else if (_XMEventHandler) (*_XMEventHandler)(&event);
		    break;
		}
		if (event_xmp == cur_p) {
		    _XMRefreshPane(menu, cur_p);
		}
		else _XMRefreshPaneText(menu, event_xmp);
		break;
	    case EnterWindow:
		event_xmw = (XMWindow *)XLookUpAssoc(
		    menu->assoc_tab,
		    event.window
		);
		if (event_xmw == NULL) break;
		if (event_xmw->type == SELECTION) {
		    /*
		     * We have entered a selection.
		     */
		    cur_s = (XMSelect *)event_xmw;
		    /*
		     * If the pane we are in is active and the
		     * selection entered is active then activate
		     * the selection.
		     */
		    if (cur_p->active && cur_s->active) {
			cur_s->activated = 1;
			_XMRefreshSelection(menu, cur_s);
		    }
		}
		else {
		    /*
		     * We have entered a pane.
		     */
		    xc_event = (XCrossingEvent *)&event;
		    status = XInterpretLocator(
			menu->parent,
			&x, &y,
			&xc_window,
			xc_event->location
		    );
		    if (status == _X_FAILURE) {
			_XMErrorCode = XME_INTERP_LOC;
			return(XM_FAILURE);
		    }
		    event_xmp = (XMPane *)XLookUpAssoc(
			menu->assoc_tab,
			xc_window
		    );
		    if (event_xmp->window == cur_p->window) break;
		    if (event_xmp->serial > cur_p->serial) forward = TRUE;
		    else forward = FALSE;
		    p_ptr = cur_p;
		    while(1) {
			if (forward) p_ptr = p_ptr->next;
			else p_ptr = p_ptr->prev;
			/*
			 * If the new pane is an active pane then
			 * activate it.
			 */
			if (p_ptr->active) {
			    p_ptr->activated = 1;
			    XChangeBackground(
				p_ptr->window,
				menu->bkgnd_pixmap
			    );
			    XClear(p_ptr->window);
			}
			/*
			 * Raise the new pane.
			 */
			XRaiseWindow(p_ptr->window);
			/*
			 * If the previous current pane was activated
			 * deactivate it.
			 */
			if (cur_p->activated) {
			    cur_p->activated = 0;
			    XChangeBackground(
				cur_p->window,
				menu->inact_pixmap
			    );
			    _XMRefreshPaneText(menu, cur_p);
			}
			/*
			 * Make the new pane the current pane.
			 */
			cur_p = p_ptr;
			/* 
			 * If we have cycled through to the event
			 * pane we are done.
			 */
			if (p_ptr->window == event_xmp->window) break;
		    }
		}
		break;
	    case LeaveWindow:
		event_xmw = (XMWindow *)XLookUpAssoc(
		    menu->assoc_tab,
		    event.window
		);
		if (event_xmw == NULL) break;
		/*
		 * If the current selection was activated then
		 * deactivate it.
		 */
		if (cur_s->activated) {
		    cur_s->activated = 0;
		    _XMRefreshSelection(menu, cur_s);
		}
		cur_s = NULL;
		break;
	    case ButtonPressed:
	    case ButtonReleased:
		*p_num = cur_p->serial;
		/*
		 * Check to see if there is a current selecion.
		 */
		if (cur_s != NULL) {
		    /*
		     * Set the selection number to the current selection.
		     */
		    *s_num = cur_s->serial;
		    /*
		     * If the current selection was activated then
		     * we have a valid selection otherwise we have
		     * an inactive selection.
		     */
		    if (cur_s->activated) {
			*data = cur_s->data;
			ret_val = XM_SUCCESS;
		    }
		    else {
			ret_val = XM_IA_SELECT;
		    }
		}
		else {
		    /*
		     * No selection was current.
		     */
		    ret_val = XM_NO_SELECT;
		}
		selection = TRUE;
		break;
	    default:
		/*
		 * If AEQ mode is enabled then queue the event.
		 */
		if (menu->aeq) {
		    feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
		    if (feq_tmp == NULL) {
			_XMErrorCode = XME_CALLOC;
			return(XM_FAILURE);
		    }
		    feq_tmp->event = event;
		    feq_tmp->next = feq;
		    feq = feq_tmp;
		}
		else if (_XMEventHandler) (*_XMEventHandler)(&event);
	}
	/*
	 * If a selection has been made, break out of the event loop.
	 */
	if (selection == TRUE) break;
    }

    /*
     * Unmap the menu.
     */
    if (saved) {
	for (
	    p_ptr = menu->p_list->next;
	    p_ptr != menu->p_list;
	    p_ptr = p_ptr->next
	) {
	    XUnmapTransparent(p_ptr->window);
	}
    }
    else {
	for (
	    p_ptr = menu->p_list->next;
	    p_ptr != menu->p_list;
	    p_ptr = p_ptr->next
	) {
	    XUnmapWindow(p_ptr->window);
	}
    }

    /*
     * Ungrab the mouse.
     */
    XUngrabMouse();

    /* 
     * Restore bits under where the menu was if we managed
     * to save them and free the pixmap.
     */
    if (saved) {
	XPixmapPut(
	    menu->parent,
	    0, 0,
	    save_x, save_y,
	    save_w, save_h,
	    save_pixmap,
	    GXcopy, AllPlanes
	);
	XFreePixmap(save_pixmap);
    }

    /*
     * Ungrab the X server.
     */
    if (menu->freeze) XUngrabServer();

    /*
     * If there is a current selection deactivate it.
     */
    if (cur_s != NULL) cur_s->activated = 0;

    /*
     * Deactivate the current pane.
     */
    cur_p->activated = 0;
    XChangeBackground(cur_p->window, menu->inact_pixmap);

    /*
     * Synchronize the X buffers and the X event queue.
     */
    XSync(0);
    
    /*
     * Dispatch any events remaining on the queue.
     */
    while (QLength()) {
	/*
	 * Fetch the next event.
	 */
	XNextEvent(&event);

	/*
	 * Discard any events left on the queue that belong to XMenu.
	 * All others are held and then returned to the event queue.
	 */
	switch (event.type) {
	    case ExposeWindow:
	    case EnterWindow:
	    case LeaveWindow:
	    case ButtonPressed:
	    case ButtonReleased:
		/*
		 * Does this event belong to one of XMenu's windows?
		 * If so, discard it and process the next event.
		 * If not fall through and treat it as a foreign event.
		 */
		event_xmp = (XMPane *)XLookUpAssoc(
		    menu->assoc_tab,
		    event.window
		);
		if (event_xmp != NULL) continue;
	    default:
		/*
		 * This is a foreign event.
		 * Queue it for later return to the X event queue.
		 */
		feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
		if (feq_tmp == NULL) {
		    _XMErrorCode = XME_CALLOC;
		    return(XM_FAILURE);
		}
		feq_tmp->event = event;
		feq_tmp->next = feq;
		feq = feq_tmp;
	}
    }

    /*
     * Return any foreign events that were queued to the X event queue.
     */
    while (feq != NULL) {
	feq_tmp = feq;
	XPutBackEvent(&feq_tmp->event);
	feq = feq_tmp->next;
	free((char *)feq_tmp);
    }

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(ret_val);
}
