#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	Move - Window movement subroutine for the X Window System window
 *	manager (xwm).
 *
 *	File:		Move.c
 */
#ifndef lint
static char *rcsid_Move_c = "$Header: Move.c,v 10.3 86/02/01 16:09:58 tony Rel $";
#endif

#include "xwm.h"

Move(window, x, y)
    Window window;
    int x;
    int y;
{
    register int prev_x;		/* Previous event window X location. */
    register int prev_y;		/* Previous event window Y location. */
    register WindowInfo window_info;	/* Event window information. */
    int cur_x;				/* Current event window X location. */
    int cur_y;				/* Current event window Y location. */
    int ulx, uly;			/* Event window upper left X and Y. */
    int lrx, lry;			/* Event window lower right X and Y. */
    int init_ulx, init_uly;		/* Init window upper left X and Y. */
    int init_lrx, init_lry;		/* Init window lower right X and Y. */
    int status;				/* Routine call status. */
    int num_vectors;			/* Number of vectors in box. */
    Window sub_window;			/* Query mouse event sub-window. */
    XButtonEvent button_event;		/* Button event packet. */
    Bool moving;			/* Are we moving the window? */
    Vertex box[MAX_BOX_VECTORS];	/* Box vertex buffer. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap effect verted buffer. */

    /*
     * Clear the vector buffers.
     */
    bzero(box, sizeof(box));
    if (Zap) bzero(zap, sizeof(zap));
    
    /*
     * Gather info on the event window since we may move it.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) {
        /*
	 * If there is a query error, abort the operation and return.
	 */
	return;
    }

    /*
     * Initialize movement variables.
     */
    ulx = window_info.x;
    init_ulx = ulx;
    uly = window_info.y;
    init_uly = uly;
    lrx = window_info.x + window_info.width + (window_info.bdrwidth << 1) - 1;
    init_lrx = lrx;
    lry = window_info.y + window_info.height + (window_info.bdrwidth << 1) - 1;
    init_lry = lry;

    moving = FALSE;

    while (TRUE) {

    	/*
	 * Check to see if we have a change in mouse button status.
	 * This is how we get out of this "while" loop.
	 */
	if (XPending() && GetButton(&button_event)) {
	    /*
	     * Process the pending events, this sequence is the only
	     * way out of the loop and the routine.
	     */
	    if (
		(button_event.type == ButtonReleased) &&
		((button_event.detail & ValueMask) == RightButton)
	    ){
		/*
		 * Ok, the right button was released.
		 * If we are not moving see if we should be.  If we
		 * have move more than Delta pixels then start moving
		 * the window.
		 */
		cur_x = button_event.x;
		cur_y = button_event.y;

		if (
		    !moving &&
		    (abs(cur_x - x) > Delta) || (abs(cur_y - y) > Delta)
		){
		    /*
		     * Start moving.
		     */
		    /*
		     * Change the cursor.
		     */
		    status = XGrabButton(
			RootWindow,
			ArrowCrossCursor,
			(RightMask | ButtonMask),
			(ButtonPressed | ButtonReleased)
		    );
		    if (status == FAILURE) {
			Error("Move -> Unable to grab right button and change cursor.");
		    }

		    /*
		     * Set the moving flag.
		     */
		    moving = TRUE;
		}

		if (moving) {
		    /*
		     * We are moveing so reset the cursor and move the
		     * window.
		     */
		    status = XGrabButton(
			RootWindow,
			CircleCursor,
			(RightMask | ButtonMask),
			(ButtonPressed | ButtonReleased)
		    );
		    if (status == FAILURE) {
			Error("Move -> Unable to grab right button and change cursor.");
		    }

		    if (Zap) {
			num_vectors = StoreZap(
			    zap,
			    init_ulx, init_uly,
			    init_lrx, init_lry,
			    ulx, uly,
			    lrx, lry
			);
			XDraw(
			    RootWindow,
			    zap, num_vectors,
			    DRAW_HEIGHT, DRAW_WIDTH,
			    DRAW_VALUE, DRAW_FUNC, DRAW_PLANES
			);
			XDraw(
			    RootWindow,
			    zap, num_vectors,
			    DRAW_HEIGHT, DRAW_WIDTH,
			    DRAW_VALUE, DRAW_FUNC, DRAW_PLANES
			);
		    }

		    XMoveWindow(window, ulx, uly);
		    return;
		}
		else {
		    /*
		     * We didn't move so this must have been a raise
		     * window operation only. No need to reset the
		     * cursor since we only change it if we are moving.
		     */
		    XRaiseWindow(window);
		    return;
		}
	    }
	    else {
		/*
		 * Some other button event occured, this aborts the
		 * current operation.
		 */
		if (moving) {
		    /*
		     * If we were moving then reset the cursor.
		     */
		     status = XGrabButton(
			RootWindow,
			CircleCursor,
			(RightMask | ButtonMask),
			(ButtonPressed | ButtonReleased)
		    );
		    if (status == FAILURE) {
			Error("Move -> Unable to grab right button and change cursor.");
		    }
		}
		return;
	    }
	}

	if (!moving) {
	    /*
	     * If we are not moving see if we should be.  If we
	     * have move more than Delta pixels then start moving
	     * the window.
	     */
	    XQueryMouse(RootWindow, &cur_x, &cur_y, &sub_window);
	    if ((abs(cur_x - x) > Delta) || (abs(cur_y - y) > Delta)) {
		/*
		 * Start moving.
		 */

		/*
		 * Change the cursor.
		 */
		status = XGrabButton(
		    RootWindow,
		    ArrowCrossCursor,
		    (RightMask | ButtonMask),
		    (ButtonPressed | ButtonReleased)
		);
		if (status == FAILURE) {
		    Error("Move -> Unable to grab right button and change cursor.");
		}

		/*
		 * Store the box.
		 */
		if (Grid) {
		    num_vectors = StoreGridBox(box, ulx, uly, lrx, lry);
		}
		else {
		    num_vectors = StoreBox(box, ulx, uly, lrx, lry);
		}

		/*
		 * Initialize the previous location variables.
		 */
		prev_x = x;
		prev_y = y;

		/*
		 * Set the moving flag.
		 */
		moving = TRUE;
	    }
	    else {
		/*
		 * We haven't moved enough yet.
		 */
		continue;
	    }
	}

	if (moving) {
	    /*
	     * If we have begun moving or have been moving take care
	     * of all the little that have to change.
	     */
	    XQueryMouse(RootWindow, &cur_x, &cur_y, &sub_window);
	    if ((cur_x != prev_x) || (cur_y != prev_y)) {
		/*
		 * Box position has changed.
		 */
		ulx += cur_x - prev_x;
		uly += cur_y - prev_y;
		lrx += cur_x - prev_x;
		lry += cur_y - prev_y;

		/*
		 * Box needs to be restored.
		 */
		if (Grid) {
		    num_vectors = StoreGridBox(box, ulx, uly, lrx, lry);
		}
		else {
		    num_vectors = StoreBox(box, ulx, uly, lrx, lry);
		}

		/* 
		 * Save old box position.
		 */
		prev_x = cur_x;
		prev_y = cur_y;
	    }

	    /*
	     * Draw Box.
	     */
	    XDraw(
		RootWindow,
		box, num_vectors,
		DRAW_HEIGHT, DRAW_WIDTH,
		DRAW_VALUE, DRAW_FUNC, DRAW_PLANES
	    );
	    XDraw(
		RootWindow,
		box, num_vectors,
		DRAW_HEIGHT, DRAW_WIDTH,
		DRAW_VALUE, DRAW_FUNC, DRAW_PLANES
	    );
	}
    }
}
