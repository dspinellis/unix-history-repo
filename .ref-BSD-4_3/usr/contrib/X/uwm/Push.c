#ifndef lint
static char *rcsid_Push_c = "$Header: Push.c,v 10.3 86/02/01 16:23:34 tony Rel $";
#endif	lint

/************************************************************************
 *									*
 *			Copyright (c) 1986 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *		         All Rights Reserved.				*
 *									*
 *	Permission to use, copy, modify, and distribute this software	*
 *	and its documentation is hereby granted only to licensees of 	*
 *	The Regents of the University of California pursuant to their	*
 *	license agreement for the Berkeley Software Distribution 	*
 *	provided that the following appears on all copies.		*
 *									*
 *            "LICENSED FROM DIGITAL EQUIPMENT CORPORATION		*
 *                      COPYRIGHT (C) 1986				*	
 *                 DIGITAL EQUIPMENT CORPORATION			*
 *                         MAYNARD, MA					*
 *                     ALL RIGHTS RESERVED.				*
 *									*
 *      THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT	* 
 *	NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL	*
 *	EQUIPMENT CORPORATION.  DIGITAL MAKES NO REPRESENTATIONS	*
 *	ABOUT SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE. IT IS	*
 *	SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.		*
 *									*	
 * 	IF THE UNIVERSITY OF CALIFORNIA OR ITS LICENSEES MODIFY 	*	
 *	THE SOFTWARE IN A MANNER CREATING DERIVATIVE COPYRIGHT 		*	
 *	RIGHTS APPROPRIATE COPYRIGHT LEGENDS MAY BE PLACED ON THE	*
 *	DERIVATIVE WORK IN ADDITION TO THAT SET FORTH ABOVE."		*	
 *									*
 ************************************************************************/
 

/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)Push.c	3.8	1/24/86";
#endif

#include "uwm.h"

#define PUSH_DOWN	1
#define PUSH_UP		2
#define PUSH_LEFT	3
#define PUSH_RIGHT	4

extern Bool PushAll();

Bool PushDown(window, mask, button, x, y)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
{
    return(PushAll(window, PUSH_DOWN));
}

Bool PushUp(window, mask, button, x, y)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
{
    return(PushAll(window, PUSH_UP));
}

Bool PushLeft(window, mask, button, x, y)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
{
    return(PushAll(window, PUSH_LEFT));
}

Bool PushRight(window, mask, button, x, y)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
{
    return(PushAll(window, PUSH_RIGHT));
}

Bool PushAll(w, direction)
Window w;
int direction;
{
    WindowInfo winfo;			/* Event window information. */
    WindowInfo rinfo;			/* Root window information. */
    int xofs, yofs;			/* Movement offsets. */
    int x, y;				/* New window position. */

    /*
     * Do not try to move the root window.
     */
    if (w == RootWindow)
        return(FALSE);

    /*
     * Gather info on the event window.
     */
    status = XQueryWindow(w, &winfo);
    if (status == FAILURE) return(FALSE);

    /*
     * Calculate the movement offsets.
     */
    switch(direction) {
    case PUSH_DOWN:
        xofs = 0;
        yofs = Push ? (winfo.height / Pushval) : Pushval;
        break;
    case PUSH_UP:
        xofs = 0;
        yofs = 0 - (Push ? (winfo.height / Pushval) : Pushval);
        break;
    case PUSH_LEFT:
        xofs = 0 - (Push ? (winfo.width / Pushval) : Pushval);
        yofs = 0;
        break;
    case PUSH_RIGHT:
        xofs = Push ? (winfo.width / Pushval) : Pushval;
        yofs = 0;
        break;
    }

    /*
     * Calculate the new window position.
     */
    x = winfo.x + xofs;
    y = winfo.y + yofs;

    /*
     * Normalize the new window coordinates so we don't
     * lose the window off the edge of the screen.
     */
    if (x < (0 - winfo.width + CURSOR_WIDTH - (winfo.bdrwidth << 1)))
        x = 0 - winfo.width + CURSOR_WIDTH - (winfo.bdrwidth << 1);
    if (y < (0 - winfo.height + CURSOR_HEIGHT - (winfo.bdrwidth << 1)))
        y = 0 - winfo.height + CURSOR_HEIGHT - (winfo.bdrwidth << 1);
    if (x > (ScreenWidth - CURSOR_WIDTH))
        x = ScreenWidth - CURSOR_WIDTH;
    if (y > (ScreenHeight - CURSOR_HEIGHT))
        y = ScreenHeight - CURSOR_HEIGHT;

    /*
     * Move the window into place.
     */
    XMoveWindow(w, x, y);

    return(FALSE);
}
