#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	Change - This subroutine implements window size manipulation
 *	for the X Window System window manager (xwm).
 *
 */

#ifndef lint
static char *rcsid_Change_c = "$Header: Change.c,v 10.3 86/02/01 16:09:10 tony Rel $";
#endif

#include "xwm.h"

Bool Change(window, x0, y0)
    Window window;			/* Event window. */
    int x0, y0;				/* Event mouse coordinates. */
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
    int status;				/* Routine call return status. */
    int num_vectors;			/* Number of vectors to XDraw. */
    Window assoc;			/* Window represented by the icon. */
    Window sub_win;			/* Mouse query sub window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex box[MAX_BOX_VECTORS];	/* Box drawing vertex buffer. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap drawing vertex buffer. */
    Bool domult;			/* ??? */
    Bool stop;				/* Should the window stop changing? */
    Bool changing;			/* Is the window changing? */

    /*
     * Clear the vector buffers.
     */
    bzero(box, sizeof(box));
    if (Zap) bzero(zap, sizeof(zap));

    /*
     * Gather info about the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) {
	/*
	 * If there is a query error, abort the operation.
	 */
	return(FALSE);
    }

    /*
     * Check to see if we have an icon window.
     */
    if (window_info.type == IsIcon) {
	/*
	 * We have an uniconify event, wait for a change in button state.
	 */
	assoc = window_info.assoc_wind;

	/*
	 * Gather info about the assoc window.
	 */
	status = XQueryWindow(assoc, &assoc_info);
	if (status == FAILURE) {
	    /*
	     * If there is a query error, abort the operation.
	     */
	    return(FALSE);
	}

	/*
	 * Spin our wheels untill there is a button event.
	 */
	while (!GetButton(&button_event));

	/*
	 * Ok, we have a button event, process it.
	 */
	if (
	    (button_event.type == ButtonReleased) &&
	    ((button_event.detail & ValueMask) == MiddleButton)
	){
	    /*
	     * Middle button came up, this means we have to uniconify the
	     * window.
	     */

	    if (Zap) {
		/*
		 * Store the zap vector buffer.
		 */
		num_vectors = StoreZap(
		    zap,
		    assoc_info.x - 1,
		    assoc_info.y - 1,
		    assoc_info.x + assoc_info.width +
			(assoc_info.bdrwidth << 1),
		    assoc_info.y + assoc_info.height +
			(assoc_info.bdrwidth << 1),
		    window_info.x - 1,
		    window_info.y - 1,
		    window_info.x + window_info.width +
			(window_info.bdrwidth << 1),
		    window_info.y + window_info.height +
			(window_info.bdrwidth << 1)
		);
	    }
	    
	    /*
	     * Map the window and synchronize.
	     */
	    XMapWindow(assoc);

	    if (Zap) {
		/*
		 * Draw the zap lines.
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
	     * Unmap the icon window.
	     */
	    XUnmapWindow(window);

	    /*
	     * This changed the window, return TRUE.
	     */
	    return(TRUE);
	}
	else {
	    /*
	     * Some other button event occured.
	     * Don't change window, return FALSE.
	     */
	     return(FALSE);
	}
    }

    /*
     * If we are here then we have a resize operation in progress.
     */

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
	w0 = 1;
	h0 = 1;
    }
    mx = x2 - window_info.width + w0 + winc;
    my = y2 - window_info.height + h0 + hinc;
    w0 += (window_info.bdrwidth << 1) - 1;
    h0 += (window_info.bdrwidth << 1) - 1;
    x = x2;
    y = y2;
    dx = 1;
    dy = 1;
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

    stop = FALSE;
    changing = FALSE;
    ox = -1;
    oy = -1;
    lx = -1;
    ly = -1;

    while (!stop) {
	if (x != ox || y != oy) {
	    if (Grid) {
	    	num_vectors = StoreGridBox(
		    box,
		    min(x1, x), min(y1, y),
		    max(x1, x), max(y1, y)
		);
	    }
	    else {
	    	num_vectors = StoreBox(
		    box,
		    min(x1, x), min(y1, y),
		    max(x1, x), max(y1, y)
		);
	    }
	    if (domult && changing) {
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

	if (changing) {
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

	if (XPending() && GetButton(&button_event)) {
	    if (
		(button_event.type == ButtonReleased) &&
		((button_event.detail & ValueMask) == MiddleButton)
	    ){
		x = button_event.x;
		y = button_event.y;
		stop = TRUE;
	    }
	    else {
		if (domult && changing) {
		    XUnmapWindow(Pop);
		}
		return(FALSE);
	    }
	}
	else {
	    XQueryMouse(RootWindow, &x, &y, &sub_win);
	}

	if (x == lx && y == ly) {
	    x = ox;
	    y = oy;
	    continue;
	}

	lx = x;
	ly = y;

	if (!changing) {
	    if (abs(x - x0) < Delta && abs(y - y0) < Delta) {
		x = ox;
		y = oy;
		continue;
	    }
	    if (dx)
		ox = -1;
	    else
		oy = -1;
	    changing = TRUE;
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
    if (!changing) return(FALSE);
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
    return(TRUE);
}





