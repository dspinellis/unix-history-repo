#ifndef lint
static char *rcsid_Move_c = "$Header: Move.c,v 10.4 86/11/19 16:24:03 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)Move.c	3.8	1/24/86";
#endif

#include "uwm.h"

Bool Move(window, mask, button, x, y)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
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
    int num_vectors;			/* Number of vectors in box. */
    Window sub_window;			/* Query mouse event sub-window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex box[MAX_BOX_VECTORS];	/* Box vertex buffer. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap effect verted buffer. */

    /*
     * Do not try to move the root window.
     */
    if (window == RootWindow)
        return(FALSE);

    /*
     * Change the cursor.
     */
    status = XGrabButton(RootWindow, ArrowCrossCursor, mask, EVENTMASK);
    if (status == FAILURE)
        Error("Move -> Unable to grab button and change cursor.");

    /*
     * Clear the vector buffers.
     */
    bzero(box, sizeof(box));
    if (Zap) bzero(zap, sizeof(zap));
    
    /*
     * Gather info on the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) return(FALSE);

    /*
     * Initialize movement variables.
     */
    init_ulx = ulx = window_info.x;
    init_uly = uly = window_info.y;
    init_lrx = lrx = window_info.x + window_info.width +
                     (window_info.bdrwidth << 1) - 1;
    init_lry = lry = window_info.y + window_info.height +
                     (window_info.bdrwidth << 1) - 1;

    /*
     * Store the box.
     */
    if (Grid)
        num_vectors = StoreGridBox(box, ulx, uly, lrx, lry);
    else num_vectors = StoreBox(box, ulx, uly, lrx, lry);

    /*
     * Initialize the previous location variables.
     */
    prev_x = x;
    prev_y = y;

    /*
     * Freeze the server, if requested by the user.
     * This results in a solid box instead of a flickering one.
     */
    if (Freeze) XGrabServer();

    /*
     * Process any pending exposure events before drawing the box.
     */
    while (QLength() > 0) {
        XPeekEvent(&button_event);
        if (button_event.window == RootWindow)
            break;
        GetButton(&button_event);
    }

    /*
     * Now draw the box.
     */
    DrawBox();
    Frozen = window;

    /*
     * Main loop.
     */
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

            /*
             * If we froze the server, then erase the last lines drawn.
             */
            if (Freeze) {
                DrawBox();
                Frozen = (Window)0;
                XUngrabServer();
            }

	    if ((button_event.type == ButtonReleased) &&
		((button_event.detail & ValueMask) == button)) {

		/*
		 * The button was released, so reset the cursor and
		 * move the window.
		 */
                Grab(mask);

		if (Zap) {
                    num_vectors = StoreZap(zap,
                                           init_ulx, init_uly,
                                           init_lrx, init_lry,
                                           ulx, uly,
                                           lrx, lry);
                    DrawZap();
                    DrawZap();
                }
		XMoveWindow(window, ulx, uly);
		return(TRUE);
	    }
	    else {

		/*
		 * Some other button event occured, this aborts the
		 * current operation.
		 */

		/*
		 * Reset the cursor.
		 */
                Grab(mask);
		return(TRUE);
	    }
	}

        /*
         * Take care of all the little things that have changed.
         */
        XUpdateMouse(RootWindow, &cur_x, &cur_y, &sub_window);
        if ((cur_x != prev_x) || (cur_y != prev_y)) {

            /*
             * If we've frozen the server, then erase the old box first!
             */
            if (Freeze)
                DrawBox();

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
            if (Grid)
                num_vectors = StoreGridBox(box, ulx, uly, lrx, lry);
            else num_vectors = StoreBox(box, ulx, uly, lrx, lry);


            /*
             * Draw the new box.
             */
            if (Freeze)
                DrawBox();
    	}

    	/* 
    	 * Save old box position.
    	 */
    	prev_x = cur_x;
    	prev_y = cur_y;

        /*
         * If server is not frozen, then draw the "flicker" box.
         */
        if (!Freeze) {
            DrawBox();
            DrawBox();
        }
    }
}
