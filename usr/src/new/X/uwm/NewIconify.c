#ifndef lint
static char *rcsid_NewIconify_c = "$Header: NewIconify.c,v 10.5 86/11/19 16:24:13 jg Rel $";
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
static char *sccsid = "@(#)NewIconify.c	3.8	1/24/86";
#endif

#include "uwm.h"

Bool NewIconify(window, mask, button, x, y)
Window window;                          /* Event window. */
int mask;                               /* Button/key mask. */
short button;                           /* Button event detail. */
int x, y;                               /* Event mouse position. */
{
    register WindowInfo window_info;	/* Event window info. */
    register WindowInfo icon_info;	/* Icon window info. */
    char *name;				/* Event window name. */
    int mse_x, mse_y;			/* Mouse X and Y coordinates. */
    int icon_x, icon_y;			/* Icon U. L. X and Y coordinates. */
    int icon_w, icon_h;			/* Icon width and height. */
    int icon_bdr;			/* Icon border width. */
    int prev_x;				/* Previous event window X location. */
    int prev_y;				/* Previous event window Y location. */
    int cur_x;				/* Current event window X location. */
    int cur_y;				/* Current event window Y location. */
    int ulx, uly;			/* Event window upper left X and Y. */
    int lrx, lry;			/* Event window lower right X and Y. */
    int init_ulx, init_uly;		/* Init window upper left X and Y. */
    int init_lrx, init_lry;		/* Init window lower right X and Y. */
    int num_vectors;			/* Number of vectors in box. */
    int status;				/* Routine call return status. */
    Window icon;			/* Icon window. */
    Window sub_win;			/* Mouse position sub-window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex box[MAX_BOX_VECTORS];	/* Box vertex buffer. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap effect vertex buffer. */
    Bool iconifying;			/* Are we iconifying? */

    /*
     * Do not lower or iconify the root window.
     */
    if (window == RootWindow)
        return(FALSE);

    /*
     * Change the cursor to the icon cursor.
     */
    status = XGrabButton(RootWindow, ArrowCrossCursor, mask, EVENTMASK);
    if (status == FAILURE)
        Error("NewIconify -> Unable to grab button and change cursor.");

    /*
     * Clear the vector buffers.
     */
    bzero(box, sizeof(box));
    if (Zap) bzero(zap, sizeof(zap));
    
    /*
     * Get info on the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) return(FALSE);

    /*
     * Are we iconifying or de-iconifying?
     */
    if (window_info.type != IsIcon) {

        /*
         * Window => Icon (Iconifying).
         */
        /*
         * If an icon window doesn't exist for the event window, then
         * make one.
         */
        if (window_info.assoc_wind == 0) {

	    /*
             * Set the icon border width.
             */ 
            icon_bdr = IBorderWidth;

            /*
             * Determine the size of the icon window.
             */
            status = XFetchName(window, &name);
            if (status == FAILURE) return(FALSE);
            icon_h = IFontInfo.height + (VIconPad << 1);
            icon_w = XQueryWidth(name, IFont);
            if (icon_w == 0) icon_w = icon_h;
            else icon_w += (HIconPad << 1);


            /*
             * Create the icon window.
             */
            icon = XCreateWindow(RootWindow, x + (icon_w >> 1),
                                 y + (icon_h >> 1), icon_w, icon_h,
                                 icon_bdr, IBorder, IBackground);
            if (icon == FAILURE) return(FALSE);

            /*
             * Use the text cursor whenever the mouse is in the icon window.
             */
            XDefineCursor(icon, TextCursor);

            /*
             * Select "key pressed", "window exposure" and "unmap window"
             * events for the icon window.
             */
            XSelectInput(icon, (KeyPressed | ExposeWindow | UnmapWindow));

            /*
             * Set the event window's icon window to be the new icon window.
             */
            XSetIconWindow(window, icon);
        }
        else {
            /*
             * If we already have an icon window all we have to do is
             * retrieve the info on it and move it into place.
             */
            icon = window_info.assoc_wind;

            /*
             * Get info on the icon window.
             */
            status = XQueryWindow(icon, &icon_info);
            if (status == FAILURE) return(FALSE);

            /*
             * Determine the height, width, and borderwidth of the icon.
             */
            icon_h = icon_info.height;
            icon_w = icon_info.width;
            icon_bdr = icon_info.bdrwidth;
        }

        iconifying = TRUE;
    }
    else {

        /*
         * Icon => Window (DeIconifying).
         */
        /*
         * If the window is gone, destroy the icon and return.
         */
        if (window_info.assoc_wind == 0) {
            XDestroyWindow(window);
            Grab(mask);
            return(FALSE);
        }

        /*
         * We call the normal window the "icon" window only to simplify
         * the code later on in the function.
         */
        icon = window_info.assoc_wind;

        /*
         * Get info on the icon window.
         */
        status = XQueryWindow(icon, &icon_info);
        if (status == FAILURE) return(FALSE);

        /*
         * Determine the height, width, and borderwidth of the icon.
         */
        icon_h = icon_info.height;
        icon_w = icon_info.width;
        icon_bdr = icon_info.bdrwidth;

        iconifying = FALSE;
    }

    /*
     * Initialize the movement variables.
     */
    init_ulx = ulx = x - (icon_w >> 1) - icon_bdr;
    init_uly = uly = y - (icon_h >> 1) - icon_bdr;
    init_lrx = lrx = x + (icon_w >> 1) + icon_bdr - 1;
    init_lry = lry = y + (icon_h >> 1) + icon_bdr - 1;
    prev_x = x;
    prev_y = y;


    /*
     * Store the box.
     */
    if (Grid)
        num_vectors = StoreGridBox(box, ulx, uly, lrx, lry);
    else num_vectors = StoreBox(box, ulx, uly, lrx, lry);

    /*
     * Freeze the server, if requested by the user.
     * This results in a solid box instead of a flickering one.
     */
    if (Freeze)
        XGrabServer();

    /*
     * Process any outstanding events before drawing the box.
     */
    while (QLength() > 0) {
        XPeekEvent(&button_event);
        if (button_event.window == RootWindow)
            break;
        GetButton(&button_event);
    }

    /*
     * Draw the box.
     */
    DrawBox();
    if (Freeze)
        Frozen = window;

    /*
     * We spin our wheels here looking for mouse movement or a change
     * in the status of the buttons.
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

            /*
             * Save the mouse cursor location.
             */
	    mse_x = button_event.x;
	    mse_y = button_event.y;
	    break;
	}
	else {
	    /*
	     * Continue to track the mouse until we get a change
             * in button status.
	     */
            XUpdateMouse(RootWindow, &cur_x, &cur_y, &sub_win);

            /*
             * If the mouse has moved, then make sure the box follows it.
             */
            if ((cur_x != prev_x) || (cur_y != prev_y)) {

                /*
                 * If we've frozen the server, then erase the old box first!
                 */
                if (Freeze)
                    DrawBox();
    
                /*
                 * Set the new box position.
                 */
                ulx += cur_x - prev_x;
                uly += cur_y - prev_y;
                lrx += cur_x - prev_x;
                lry += cur_y - prev_y;
    
                /*
                 * Calculate the vectors for the new box.
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
             * Save the old box position.
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

    /*
     * Restore the main cursor.
     */
    Grab(mask);

    /*
     * If the button is not a button release of the same button pressed,
     * then abort the operation.
     */
    if ((button_event.type != ButtonReleased) ||
        ((button_event.detail & ValueMask) != button)) {
	    return(TRUE);
    }

    /*
     * If we are here we have committed to iconifying/deiconifying.
     */

    /*
     * Determine the coordinates of the icon or window;
     * normalize the window or icon coordinates if the user so desires.
     */
    icon_x = mse_x - (icon_w >> 1) - icon_bdr;
    icon_y = mse_y - (icon_h >> 1) - icon_bdr;
    if ((NIcon && iconifying) || (NWindow && !iconifying)) {
        if (icon_x < 0) icon_x = 0;
        if (icon_y < 0) icon_y = 0;
        if ((icon_x - 1 + icon_w + (icon_bdr << 1)) > ScreenWidth) {
            icon_x = ScreenWidth - icon_w - (icon_bdr << 1) + 1;
        }
        if ((icon_y - 1 + icon_h + (icon_bdr << 1)) > ScreenHeight) {
            icon_y = ScreenHeight - icon_h - (icon_bdr << 1) + 1;
        }
    }

    /*
     * Move the window into place.
     */
    XMoveWindow(icon, icon_x, icon_y);

    /*
     * Map the icon window.
     */
    XMapWindow(icon);

    if (Zap) {
        num_vectors = StoreZap(zap, window_info.x, window_info.y,
                               window_info.x + window_info.width +
                               (window_info.bdrwidth << 1),
                               window_info.y + window_info.height +
                               (window_info.bdrwidth << 1),
                               ulx, uly, lrx, lry);
        DrawZap();
        DrawZap();
    }

    /*
     * Unmap the event window.
     */
    XUnmapWindow(window);
    return(TRUE);
}
