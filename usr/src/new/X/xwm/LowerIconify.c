#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	LowerIconify - This subroutine implements window lowering and
 *	iconifing for the X Window System window manager (xwm).
 *
 *	File:		LowerIconify.c
 *	Modified:	ADF	29-May-85	Converted to X version 8.0
 */

#include "xwm.h"
#ifndef lint
static char *rcsid_LowerIconify_c = "$Header: LowerIconify.c,v 10.4 86/02/01 16:09:38 tony Rel $";
#endif

LowerIconify(window, x, y)
    Window window;			/* Event window. */
    int x, y;				/* Event mouse position. */
{
    register WindowInfo window_info;	/* Event window info. */
    register WindowInfo icon_info;	/* Icon window info. */
    char *name;				/* Event window name. */
    int mse_x, mse_y;			/* Mouse X and Y coordinates. */
    int icon_x, icon_y;			/* Icon U. L. X and Y coordinates. */
    int icon_w, icon_h;			/* Icon width and height. */
    int icon_bdr;			/* Icon border width. */
    int status;				/* Routine call return status. */
    int num_vectors;			/* Number of vectors in zap buffer. */
    Window icon;			/* Icon window. */
    Window sub_win;			/* Mouse position sub-window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap effect vertex buffer. */
    Bool iconifying;			/* Are we iconifying? */

    /*
     * Clear the vector buffer.
     */
    if (Zap) bzero(zap, sizeof(zap));
    
    /*
     * Get info on the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) return;

    /*
     * If the mouse is in an icon window simply lower the window and return.
     */
    if (window_info.type == IsIcon) {

	/*
	 * Wait for the button to come back up.
	 */
	while (!GetButton(&button_event));

	if (
	    (button_event.type == ButtonReleased) &&
	    ((button_event.detail & ValueMask) == LeftButton)
	){
	    XLowerWindow(window);
	}

	return;
    }

    /*
     * If we are here then the event was not on an iconified window.
     * This flag denotes whether the window we're on is being
     * iconified or not.
     */
    iconifying = FALSE;

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
	    mse_x = button_event.x;
	    mse_y = button_event.y;
	    if (
	        !iconifying &&
		((abs(mse_x - x) > Delta) || (abs(mse_y - y) > Delta))
	    ){
		/*
		 * The mouse has moved Delta pixels without the left button
		 * going up.  This means we are iconifying a window.
		 * We warn the user by changing the mouse cursor to the
		 * funny icon cursor.
		 */
		iconifying = TRUE;
		status = XGrabButton(
		    RootWindow,
		    IconCursor,
		    (LeftMask | ButtonMask),
		    (ButtonPressed | ButtonReleased)
		);
		if (status == FAILURE) {
		    Error("LowerIconify -> Unable to grab left button and change cursor.");
		}
	    }
	    break;
	}
	else if (!iconifying) {
	    /*
	     * Continue to track the mouse untill we exceed delta or
	     * get a change in button status.
	     */
	    XQueryMouse(RootWindow, &mse_x, &mse_y, &sub_win);

	    if ((abs(mse_x - x) > Delta) || (abs(mse_y - y) > Delta)) {
		/*
		 * The mouse has moved Delta pixels without the left button
		 * going up.  This means we are iconifying a window.
		 * We warn the user by changing the mouse cursor to the
		 * funny icon cursor.
		 */
		iconifying = TRUE;
		status = XGrabButton(
		    RootWindow,
		    IconCursor,
		    (LeftMask | ButtonMask),
		    (ButtonPressed | ButtonReleased)
		);
		if (status == FAILURE) {
		    Error("LowerIconify -> Unable to grab left button and change cursor.");
		}
	    }
	}
    }

    if (iconifying) {
	/*
	 * Restore the window manager mouse cursor.
	 */
	status = XGrabButton(
	    RootWindow,
	    DotCursor,
	    (LeftMask | ButtonMask),
	    (ButtonPressed | ButtonReleased)
	);
	if (status == FAILURE) {
	    Error("LowerIconify -> Unable to grab left button and change cursor.");
	}
    }

    /*
     * If the left button is released and we are not iconifying then
     * lower the window and return.  If any other button event occured
     * abort the operation.
     */
    if (
	(button_event.type == ButtonReleased) &&
	((button_event.detail & ValueMask) == LeftButton)
    ){
	if (!iconifying) {
	    XLowerWindow(window);
	    return;
	}
    }
    else {
	/*
	 * Abort!
	 */
	return;
    }

    /*
     * If we are here we have committed to iconifying the window.
     */

    if (window_info.assoc_wind == 0) {
	/*
	 * We need to make an icon window for the event window.
	 */ 

	/*
	 * Set the icon border width.
	 */ 
	icon_bdr = IBorderWidth;

	/*
	 * Determine the size of the icon window.
	 */ 
	status = XFetchName(window, &name);
	if (status == FAILURE) return;
	icon_h = IFontInfo.height + (IPadding << 1);
	icon_w = XQueryWidth(name, IFont);
	if (icon_w == 0) {
	    icon_w = icon_h;
	}
	else {
	    icon_w += (IPadding << 1);
	}

	/*
	 * Determine the coordinates of the icon window;
	 * normalize so that we don't lose the icon off the
	 * edge of the screen.
	 */
	icon_x = mse_x - (icon_w >> 1) + 1;
	if (icon_x < 0) icon_x = 0;
	icon_y = mse_y - (icon_h >> 1) + 1;
	if (icon_y < 0) icon_y = 0;
	if ((icon_x - 1 + icon_w + (icon_bdr << 1)) > ScreenWidth) {
	    icon_x = ScreenWidth - icon_w - (icon_bdr << 1) + 1;
	}
	if ((icon_y - 1 + icon_h + (icon_bdr << 1)) > ScreenHeight) {
	    icon_y = ScreenHeight - icon_h - (icon_bdr << 1) + 1;
	}

	/*
	 * Create the icon window.
	 */
	icon = XCreateWindow(
	    RootWindow,
	    icon_x, icon_y,
	    icon_w, icon_h,
	    icon_bdr,
	    IBorder, IBackground
	);
	if (icon == FAILURE) return;

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
	if (status == FAILURE) return;

	/*
	 * Set the icon border width.
	 */
	icon_bdr = icon_info.bdrwidth;

	/*
	 * Determine the size of the icon window.
	 */
	icon_h = icon_info.height;
	icon_w = icon_info.width;

	/*
	 * Determine the coordinates of the icon window;
	 * normalize so that we don't lose the icon off the
	 * edge of the screen.
	 */
	icon_x = mse_x - (icon_w >> 1) + 1;
	if (icon_x < 0) icon_x = 0;
	icon_y = mse_y - (icon_h >> 1) + 1;
	if (icon_y < 0) icon_y = 0;
	if ((icon_x - 1 + icon_w + (icon_bdr << 1)) > ScreenWidth) {
	    icon_x = ScreenWidth - icon_w - (icon_bdr << 1) + 1;
	}
	if ((icon_y - 1 + icon_h + (icon_bdr << 1)) > ScreenHeight) {
	    icon_y = ScreenHeight - icon_h - (icon_bdr << 1) + 1;
        }

	/*
	 * Move the window into place.
	 */
	XMoveWindow(icon, icon_x, icon_y);
    }

    if (Zap) {
	/*
	 * Store the zap effect vectors.
	 */
	num_vectors = StoreZap(
	    zap,
	    window_info.x - 1,
	    window_info.y - 1,
	    window_info.x + window_info.width + (window_info.bdrwidth << 1),
	    window_info.y + window_info.height + (window_info.bdrwidth << 1),
	    icon_x - 1,
	    icon_y - 1,
	    icon_x + icon_w + (icon_bdr << 1),
	    icon_y + icon_h + (icon_bdr << 1)
	);
    }

    /*
     * Map the icon window.
     */
    XMapWindow(icon);

    if (Zap) {
	/*
	 * Draw zap lines from the window to its icon.
	 */
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

    /*
     * Unmap the event window.
     */
    XUnmapWindow(window);
}
