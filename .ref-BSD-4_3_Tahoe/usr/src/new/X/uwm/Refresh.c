#ifndef lint
static char *rcsid_Refresh_c = "$Header: Refresh.c,v 10.4 86/11/19 16:24:31 jg Rel $";
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
static char *sccsid = "@(#)Refresh.c	3.8	1/24/86";
#endif

#include "uwm.h"

Bool Refresh(window, mask, button, x, y)
Window window;                          /* Event window. */
int mask;                               /* Button/key mask. */
short button;                           /* Button event detail. */
int x, y;                               /* Event mouse position. */
{
    WindowInfo winfo;			/* Root window info. */
    Window w;				/* Refresh window. */

    /*
     * Get info on the root window.
     */
    status = XQueryWindow(RootWindow, &winfo);
    if (status == FAILURE) Error("Refresh -> Can't query root window.");

    /*
     * Create and map a window which covers the root window, then destroy it.
     */
    if ((w = XCreateWindow(RootWindow, 0, 0, winfo.width, winfo.height, 0,
                           (Pixmap) 0, (Pixmap) 0)) == NULL)
        Error("Refresh -> Can't create refresh window.");
    XMapWindow(w);
    XDestroyWindow(w);
    XFlush();

    return(FALSE);
}
