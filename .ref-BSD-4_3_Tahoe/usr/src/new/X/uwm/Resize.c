#ifndef lint
static char *rcsid_Resize_c = "$Header: Resize.c,v 10.4 86/11/19 16:24:34 jg Rel $";
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
static char *sccsid = "@(#)Resize.c	3.8	1/24/86";
#endif

#include "uwm.h"

Bool Resize(window, mask, button, x0, y0)
Window window;                          /* Event window. */
int mask;                               /* Button/key mask. */
short button;                           /* Button event detail. */
int x0, y0;                             /* Event mouse position. */
{
    register WindowInfo window_info;	/* Event window info. */
    register WindowInfo assoc_info;	/* Icon's associated window info. */
    int d;				/* ??? */
    int t;				/* ??? */
    int x, y;				/* ??? */
    int h0, hinc, w0, winc, wadd, hadd;	/* ??? */
    int x1, y1, x2, y2;			/* ??? */
    int mx, my;				/* ??? */
    int ox, oy;				/* ??? */
    int lx, ly;				/* ??? */
    int pop_x, pop_y;			/* ??? */
    int hsize, vsize;			/* ??? */
    int dx, dy;				/* ??? */
    int num_vectors;			/* Number of vectors to XDraw. */
    Window assoc;			/* Window represented by the icon. */
    Window sub_win;			/* Mouse query sub window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex box[MAX_BOX_VECTORS];	/* Box drawing vertex buffer. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap drawing vertex buffer. */
    Bool domult;			/* ??? */
    Bool stop;				/* Should the window stop changing? */

    /*
     * Do nothing if the event window is the root window.
     */
    if (window == RootWindow)
        return(FALSE);

    /*
     * Gather info about the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) return(FALSE);

    /*
     * Do not resize an icon window. 
     */
    if (window_info.type == IsIcon)
        return(FALSE);

    /*
     * Clear the vector buffers.
     */
    bzero(box, sizeof(box));
    if (Zap) bzero(zap, sizeof(zap));

    /*
     * If we are here then we have a resize operation in progress.
     */

    /*
     * Turn on the resize cursor.
     */
    status = XGrabButton(RootWindow, ArrowCrossCursor, mask, EVENTMASK);
    if (status == FAILURE) {
        Error("Resize -> Unable to grab button and change cursor.");
    }

    /*
     * Get the event window resize hint.
     */
    XGetResizeHint(window, &w0, &h0, &winc, &hinc);

    /*
     * If I ever have the time to REALLY figure the rest of this out I will
     * comment it better.
     */
    wadd = winc >> 1;
    hadd = hinc >> 1;
    x1 = window_info.x;
    y1 = window_info.y;
    x2 = x1 + window_info.width + (window_info.bdrwidth << 1) - 1;
    y2 = y1 + window_info.height + (window_info.bdrwidth << 1) - 1;
    domult = (winc > 3 && hinc > 3 &&
	     (window_info.width - w0) % winc == 0 &&
	     (window_info.height - h0) % hinc == 0) ? TRUE : FALSE;
    if (w0 == 0 && winc == 1 && h0 == 0 && hinc == 1) {
	w0 = h0 = 1;
    }
    mx = x2 - window_info.width + w0 + winc;
    my = y2 - window_info.height + h0 + hinc;
    w0 += (window_info.bdrwidth << 1) - 1;
    h0 += (window_info.bdrwidth << 1) - 1;
    x = x2;
    y = y2;
    dx = dy = 1;
    if (x0 - x1 < x2 - x0) {
	dx = -1;
	x = x1;
	mx = x2 - (mx - x1);
	t = x1; x1 = x2; x2 = t;
    }
    if (y0 - y1 < y2 - y0) {
	dy = -1;
	y = y1;
	my = y2 - (my - y1);
	t = y1; y1 = y2; y2 = t;
    }
    ox = ((x0 - window_info.x - window_info.bdrwidth) * 3) /
        window_info.width;
    oy = ((y0 - window_info.y - window_info.bdrwidth) * 3) /
        window_info.height;
    if (window_info.width > 2 && window_info.height > 2 && ((ox + oy) & 1)) {
	if (ox & 1)
	    dx = 0;
	else
	    dy = 0;
    }

    if (Grid) {
    	num_vectors = StoreGridBox(
	    box,
	    MIN(x1, x), MIN(y1, y),
	    MAX(x1, x), MAX(y1, y)
	);
    }
    else {
    	num_vectors = StoreBox(
	    box,
	    MIN(x1, x), MIN(y1, y),
	    MAX(x1, x), MAX(y1, y)
	);
    }

    /*
     * If we freeze the server, then we will draw solid
     * lines instead of flickering ones during resizing.
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

    stop = FALSE;
    ox = oy = lx = ly = -1;

    while (!stop) {
	if (x != ox || y != oy) {

            /*
             * If we've frozen the server, then erase
             * the old box.
             */
            if (Freeze)
                DrawBox();

	    if (Grid) {
	    	num_vectors = StoreGridBox(
		    box,
		    MIN(x1, x), MIN(y1, y),
		    MAX(x1, x), MAX(y1, y)
		);
	    }
	    else {
	    	num_vectors = StoreBox(
		    box,
		    MIN(x1, x), MIN(y1, y),
		    MAX(x1, x), MAX(y1, y)
		);
	    }

            if (Freeze)
                DrawBox();

	    if (domult) {
		hsize = (abs(x - x1) - w0) / winc;
		vsize = (abs(y - y1) - h0) / hinc;
		PText[0] = hsize / 100 + '0';
		PText[1] = (hsize / 10) % 10 + '0';
		PText[2] = hsize % 10 + '0';
		PText[4] = vsize / 100 + '0';
		PText[5] = (vsize / 10) % 10 + '0';
		PText[6] = vsize % 10 + '0';

		/*
		 * If the font is not fixed width we have to
		 * clear the window to guarantee that the characters
		 * that were there before are erased.
		 */
		if (!(PFontInfo.fixedwidth)) XClear(Pop);
		XTextPad (
		    Pop,
		    PPadding, PPadding,
		    PText, PTextSize,
		    PFont, 0, 0, 
		    PTextForground, PTextBackground,
		    GXcopy, AllPlanes
		);
	    }
	    ox = x;
	    oy = y;
	}

        if (!Freeze) {
            DrawBox();
            DrawBox();
        }

	if (XPending() && GetButton(&button_event)) {

            if (Freeze) {
                DrawBox();
                Frozen = (Window)0;
                XUngrabServer();
            }

	    if (
		(button_event.type == ButtonReleased) &&
                ((button_event.detail & ValueMask) == button)
	    ){
		x = button_event.x;
		y = button_event.y;
		stop = TRUE;
	    }
	    else {
		if (domult) {
		    XUnmapWindow(Pop);
		}
                Grab(mask);
		return(TRUE);
	    }
	}
	else {
	    XUpdateMouse(RootWindow, &x, &y, &sub_win);
	}

	if (x == lx && y == ly) {
	    x = ox;
	    y = oy;
	    continue;
	}

	lx = x;
	ly = y;

        if (dx)
            ox = -1;
        else
            oy = -1;
        if (domult) {
            if (dx > 0)
                pop_x = x1 + window_info.bdrwidth;
            else if (dx < 0)
                pop_x = x1 - PWidth - window_info.bdrwidth + 1;
            else
                pop_x = window_info.x + window_info.bdrwidth +
                 (window_info.width - PWidth) / 2;
            if (dy > 0)
                pop_y = y1 + window_info.bdrwidth;
            else if (dy < 0)
                pop_y = y1 - PHeight - window_info.bdrwidth + 1;
            else
                pop_y = window_info.y + window_info.bdrwidth +
                 (window_info.height - PHeight) / 2;
            XMoveWindow(Pop, pop_x, pop_y);
            XMapWindow(Pop);
        }

	if (dx) {
	    if ((d = abs(x - x0) + wadd) < 0)
		d = 0;
	    d = (d / winc) * winc;
	    if (x < x0) {
		x = x2 - d;
		if (dx > 0 && x < mx)
		    x = mx;
	    } else {
		x = x2 + d;
		if (dx < 0 && x > mx)
		    x = mx;
	    }
	} else
	    x = ox;
	if (dy) {
	    if ((d = abs(y - y0) + hadd) < 0)
		d = 0;
	    d = (d / hinc) * hinc;
	    if (y < y0) {
		y = y2 - d;
		if (dy > 0 && y < my)
		    y = my;
	    } else {
		y = y2 + d;
		if (dy < 0 && y > my)
		    y = my;
	    }
	} else
	    y = oy;
    }
    if (x == x2 && y == y2) {
	XUnmapWindow(Pop);
	XRaiseWindow(window);
    } else {
	if (x < x1) {
	    t = x1; x1 = x; x = t;
	}
	if (y < y1) {
	    t = y1; y1 = y; y = t;
	}
	dx = x - x1 + 1 - (window_info.bdrwidth << 1);
	dy = y - y1 + 1 - (window_info.bdrwidth << 1);
	if (
	    (PWidth > window_info.width) ||
	    (PHeight > window_info.height) ||
	    (PWidth > dx) || 
	    (PHeight > dy)
	) {
	    XUnmapWindow(Pop);
	}
	else {
	    XUnmapTransparent(Pop);
	}
	XConfigureWindow(window, x1, y1, dx, dy);
    }
    Grab(mask);
    return(TRUE);
}
