#ifndef lint
static char *rcsid_Iconify_c = "$Header: Iconify.c,v 10.5 86/11/19 16:23:46 jg Rel $";
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
static char *sccsid = "@(#)Iconify.c	3.9	1/29/86";
#endif
 
#include "uwm.h"
 
Bool Iconify(window, mask, button, x, y)
Window window;                          /* Event window. */
int mask;                               /* Button/key mask. */
short button;                           /* Button event detail. */
int x, y;                               /* Event mouse position. */
{
    register WindowInfo window_info;	/* Event window info. */
    register WindowInfo assoc_info;	/* Associated window info. */
    char *name;				/* Event window name. */
    int mse_x, mse_y;			/* Mouse X and Y coordinates. */
    int icon_x, icon_y;			/* Icon U. L. X and Y coordinates. */
    int icon_w, icon_h;			/* Icon width and height. */
    int icon_bdr;			/* Icon border width. */
    int num_vectors;			/* Number of vectors in zap buffer. */
    Window assoc;			/* Associated window. */
    Window sub_win;			/* Mouse position sub-window. */
    XButtonEvent button_event;		/* Button event packet. */
    Vertex zap[MAX_ZAP_VECTORS];	/* Zap effect vertex buffer. */
    Bool iconifying;			/* Are we iconifying? */
 
    /*
     * Do not try to iconify the root window.
     */
    if (window == RootWindow)
        return(FALSE);
 
    /*
     * Clear the vector buffer.
     */
    if (Zap) bzero(zap, sizeof(zap));
 
    /*
     * Get the mouse cursor position in case we must put a new
     * icon there.
     */
    XQueryMouse(RootWindow, &mse_x, &mse_y, &sub_win);
 
    /*
     * Get info on the event window.
     */
    status = XQueryWindow(window, &window_info);
    if (status == FAILURE) return(FALSE);
 
    /*
     * If the event window is an icon, de-iconify it and return.
     */
    if (window_info.type == IsIcon) {
 
        assoc = window_info.assoc_wind;
 
        /*
         * Gather info about the assoc window.
         */
        status = XQueryWindow(assoc, &assoc_info);
        if (status == FAILURE) return(FALSE);
 
        /*
         * Store the zap vector buffer.
         */
        if (Zap) {
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
                        (window_info.bdrwidth << 1));
        }
 
        /*
         * Map the window and synchronize.
         */
        XMapWindow(assoc);
 
            if (Zap) {
                /*
                 * Draw the zap lines.
                 */
                DrawZap();
            }
 
        /*
         * Unmap the icon window.
         */
        XUnmapWindow(window);
 
        return(FALSE);
    }
    else {
        /*
         * If event window doesn't already have an icon window,
         * make one for it.
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
            if (icon_w == 0)
                icon_w = icon_h;
            else icon_w += (HIconPad << 1);
 
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
            assoc = XCreateWindow(
                RootWindow,
                icon_x, icon_y,
                icon_w, icon_h,
                icon_bdr,
                IBorder, IBackground
            );
            if (assoc == FAILURE) return(FALSE);
 
            /*
             * Use the text cursor whenever the mouse is in the icon window.
             */
            XDefineCursor(assoc, TextCursor);
    
            /*
             * Select "key pressed", "window exposure" and "unmap window"
             * events for the icon window.
             */
            XSelectInput(assoc, (KeyPressed | ExposeWindow | UnmapWindow));
    
            /*
             * Set the event window's icon window to be the new icon window.
             */
            XSetIconWindow(window, assoc);
        }
        else {
            /*
             * If we already have an icon window all we have to do is
             * map it.
             */
            assoc = window_info.assoc_wind;
            status = XQueryWindow(assoc, &assoc_info);
            if (status == FAILURE) return(FALSE);
            icon_x = assoc_info.x;
            icon_y = assoc_info.y;
            icon_w = assoc_info.width;
            icon_h = assoc_info.height;
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
        XMapWindow(assoc);
 
        if (Zap) {
        /*
         * Draw zap lines from the window to its icon.
         */
            DrawZap();
            DrawZap();
        }
 
        /*
         * Unmap the event window.
         */
        XUnmapWindow(window);
    }
    return(FALSE);
}
