/*************************************************************************
 * 
 * (c)Copyright 1992 Hewlett-Packard Co.,  All Rights Reserved.
 * 
 *                          RESTRICTED RIGHTS LEGEND
 * Use, duplication, or disclosure by the U.S. Government is subject to
 * restrictions as set forth in sub-paragraph (c)(1)(ii) of the Rights in
 * Technical Data and Computer Software clause in DFARS 252.227-7013.
 * 
 *                          Hewlett-Packard Company
 *                          3000 Hanover Street
 *                          Palo Alto, CA 94304 U.S.A.
 * 
 * Rights for non-DOD U.S. Government Departments and Agencies are as set
 * forth in FAR 52.227-19(c)(1,2).
 *
 *************************************************************************/

/*''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Copyright (c) 1988 by Hewlett-Packard Company
Copyright (c) 1987, 1988 by Digital Equipment Corporation, Maynard, 
              Massachusetts, and the Massachusetts Institute of Technology, 
              Cambridge, Massachusetts

Permission to use, copy, modify, and distribute this software 
and its documentation for any purpose and without fee is hereby 
granted, provided that the above copyright notice appear in all 
copies and that both that copyright notice and this permission 
notice appear in supporting documentation, and that the names of 
Hewlett-Packard, Digital or  M.I.T.  not be used in advertising or 
publicity pertaining to distribution of the software without specific, 
written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''*/


#include "hpext.h"
#include "hildef.h"
#include "scrnintstr.h"
#include "cursorstr.h"
#include "regionstr.h"
#include "inputstr.h"
#include "opaque.h"
#include "hppriv.h"

extern int hpActiveScreen;		/* Stacked mode, >1 head */
extern WindowPtr *WindowTable;		/* Defined by DIX */

static BoxRec LimitTheCursor;
static CursorPtr currentCursors[MAXSCREENS];

void hpBlottoCursors()
{
  int j;
  for (j = MAXSCREENS; j--; ) currentCursors[j] = NULL;
}

/************************************************************
 * hpConstrainCursor
 *
 * This function simply sets the box to which the cursor 
 * is limited.  
 * 
 * ASSUMPTION:  a single BoxRec is used for recording
 * the cursor limits, instead of one per screen.  This is 
 * done, in part, because the bogus hpConstrainXY routine
 * (see below) is not passed a pScreen pointer.
 *
 * THEREFORE:  Zaphod mode code will have to call this routine
 * to establish new limits when the cursor leaves one screen
 * for another.
 *
 ************************************************************/

void 
hpConstrainCursor (pScreen,pBox)
ScreenPtr pScreen;    /* Screen to which it should be constrained */
BoxPtr   pBox;        /* Box in which... */
{
	LimitTheCursor = *pBox;
}

/************************************************************
 * hpConstrainXY
 *
 * This function is called directly from x_hil.c
 * It adjusts the cursor position to fit within the current 
 * constraints.
 *
 ************************************************************/

Bool
hpConstrainXY(px,py)
    int *px, *py;
{
    *px = max( LimitTheCursor.x1, min( LimitTheCursor.x2,*px));
    *py = max( LimitTheCursor.y1, min( LimitTheCursor.y2,*py));
    return TRUE;
}

/************************************************************
 * hpCursorLimits
 *      Return a box within which the given cursor may move on the given
 *      screen. We assume that the HotBox is actually on the given screen,
 *      since dix knows that size.
 *
 * Results:
 *      A box for the hot spot corner of the cursor.
 ************************************************************/

void
hpCursorLimits( pScreen, pCursor, pHotBox, pResultBox)
ScreenPtr pScreen;       /* Screen on which limits are desired */
CursorPtr pCursor;       /* Cursor whose limits are desired */
BoxPtr    pHotBox;       /* Limits for pCursor's hot point */
BoxPtr    pResultBox;    /* RETURN: limits for hot spot */
{
    *pResultBox = *pHotBox;
    pResultBox->x2 = min(pResultBox->x2,pScreen->width);
    pResultBox->y2 = min(pResultBox->y2,pScreen->height);
}

/************************************************************
 * hpSetCursorPosition
 ************************************************************/

Bool 
hpSetCursorPosition(pScreen, xhot, yhot, generateEvent)
ScreenPtr pScreen;
short	  xhot;
short	  yhot;
Bool	  generateEvent;
{
    HPInputDevice	*InDev;			/* Input device structure */
    hpPrivScreenPtr	php;			/* XOS private structure */

    php 	= (hpPrivScreenPtr) pScreen->devPrivate;

    /* Check to see if we've switched screens: */
    InDev = GET_HPINPUTDEVICE((DeviceIntPtr)LookupPointerDevice());
    if (pScreen != InDev->pScreen)
    {
        WindowPtr pRootWindow = WindowTable[InDev->pScreen->myNum];

        /*
        ********************************************************************
        ** Turn old cursor off, blank/unblank screens for stacked mode,
        ** let DIX know there is a new screen, set the input driver variable
        ** to the new screen number.
        ********************************************************************
        */

        (*((hpPrivScreenPtr)(InDev->pScreen->devPrivate))->CursorOff)
        (InDev->pScreen);                     /* Old cursor off */
        php->ChangeScreen(pScreen);      /* Stacked mode switch */
        NewCurrentScreen(pScreen, xhot, yhot);/* Let DIX know */
        hpActiveScreen = pScreen->myNum;      /* Input driver global */
    }


    /* Must Update the Corvallis Input Driver's Variables: */
    InDev->pScreen = pScreen;
    InDev->coords[0] = xhot;
    InDev->coords[1] = yhot;

    if (!generateEvent)
    {
	(*php->MoveMouse)(pScreen, xhot, yhot, 0); /* Do the move now */
    }
    else
    {
        queue_motion_event(InDev);  /* Enqueue motion event, in x_hil.c */
        isItTimeToYield++;          /* Insures client get the event! */
    }

    return(TRUE);

}
