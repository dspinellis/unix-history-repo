#ifdef	sun
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef	lint
static char sccsid[] = "@(#)cursor.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* cursor.c	various stuff with the mouse & cursor
 *
 *	StoreCursor		Creates a cursor
 *	FreeCursor		Frees the storage taken by a cursor
 *	LoadCursor		Loads a bitmap to use as cursor
 *	InitMouse		Initialize the mouse
 *	SetCursorPosition	Forces cursor to a particular position
 *	SetMouseCharacteristics	Controls speed of cursor relative to mouse
 *
 */

/*
 *	ToDo:
 *		Threshold/Acceleration
 *		Use macros for CheckCursor()
 */

#include "Xsun.h"

extern struct pixrect *PixRect;
extern char *Xalloc();
extern DEVICE *CurrentDevice;
CURSOR *CurrentCursor;
int CursorDisplayed;
static struct pixrect *CursorPixrect;

CURSOR *StoreCursor (func, image, fore, back, mask, xoff, yoff)
	BITMAP *image;
	BITMAP *mask;
	int func, fore, back, xoff, yoff;
{
    register CURSOR *cursor;
    register CursPriv *data;

    if (!image)
	return (NULL);
    cursor = (CURSOR *) Xalloc(sizeof(CURSOR));
    cursor->width = image->width;
    cursor->height = image->height;
    /* Only the tip need be on-screen */
    cursor->xmin = 0;
    cursor->ymin = 0;
    cursor->xoff = xoff;
    cursor->yoff = yoff;
    cursor->xmax = PixRect->pr_size.x;
    cursor->ymax = PixRect->pr_size.y;
    cursor->refcnt = 1;
    data = (CursPriv *) Xalloc(sizeof(CursPriv));
    cursor->data = (caddr_t) data;
    data->top = mem_point(image->width, image->height, 1, image->data);
    data->top_bit = image;
    image->refcnt++;
    if (mask) {
	data->bot = mem_point(mask->width, mask->height, 1, mask->data);
	data->bot_bit = mask;
	mask->refcnt++;
    } else {
	data->bot = NULL;
	data->bot_bit = NULL;
    }
    data->sv = mem_create(cursor->width, cursor->height, PixRect->pr_depth);
    data->fore = fore;
    data->back = back;
    return (cursor);
}

FreeCursor (cursor)
	register CURSOR *cursor;
{
    CursPriv   *cp = CDATA(cursor);
    pr_destroy(cp->top);
    if (cp->top_bit && --(cp->top_bit)->refcnt <= 0)
	FreeBitmap(cp->top_bit);
    if (cp->bot) {
	pr_destroy(cp->bot);
	if (cp->bot_bit && --(cp->bot_bit)->refcnt <= 0)
	    FreeBitmap(cp->bot_bit);
    }
    pr_destroy(cp->sv);
    free((caddr_t) CDATA(cursor));
    free((caddr_t) cursor);
}

struct pr_prpos topbatch, botbatch;

LoadCursor (cursor)
	register CURSOR *cursor;
{
    if (CursorDisplayed)
	DisplayCursor(NULL);
    if ((CurrentCursor = cursor) != NULL) {
	topbatch.pr = CDATA(cursor)->top;
	topbatch.pos.x = 0;
	topbatch.pos.y = 0;
	botbatch.pr = CDATA(cursor)->bot;
	botbatch.pos.x = 0;
	botbatch.pos.y = 0;
	DisplayCursor(cursor);
    }
}

InitMouse ()
{
}

SetCursorPosition(pos)
	register vsCursor *pos;
{
    if (pos->x != (CurrentDevice->mouse->x) || pos->y != (CurrentDevice->mouse->y)) {
	if (CursorDisplayed)
	    DisplayCursor(NULL);
	*(CurrentDevice->mouse) = *pos;
	DisplayCursor(CurrentCursor);
    }
}

SetMouseCharacteristics (threshold, accelaration)
	int threshold, accelaration;
{
}

DisplayCursor(cs)
CURSOR *cs;
{
    extern struct pixrect *PixRect;
    vsCursor   *ms = CurrentDevice->mouse;

    if (cs == NULL) {
	if (CurrentCursor) {
	    /* take it out */
	    extern private_czmask;
	    int old_zmask = private_czmask, allmask = -1;

	    SetZmask(PixRect, &allmask);
	    pr_rop(PixRect, ms->x, ms->y, CurrentCursor->width,
		   CurrentCursor->height, PIX_SRC, CDATA(CurrentCursor)->sv, 0, 0);
	    SetZmask(PixRect, &old_zmask);
	    CursorDisplayed = 0;
	}
    }
    else {
	CursPriv   *cp = CDATA(cs);
	/* put it in */

	pr_rop(cp->sv, 0, 0, cs->width, cs->height, PIX_SRC,
	       PixRect, ms->x, ms->y);
	if (PixRect->pr_depth == 1) {
	    int botop = (cp->back == 1 ? PIX_SRC | PIX_DST : PIX_NOT(PIX_SRC) & PIX_DST);
	    int topop = (cp->fore == 1 ? PIX_SRC | PIX_DST : PIX_NOT(PIX_SRC) & PIX_DST);

	    if (botbatch.pr)
		pr_batchrop(PixRect, ms->x, ms->y,
			    botop,
			    &botbatch, 1);
	    if (topbatch.pr)
		pr_batchrop(PixRect, ms->x, ms->y,
			    topop,
			    &topbatch, 1);
	}
	else {
	    extern private_bgcolor, private_fgcolor, private_czmask;
	    int old_zmask = private_czmask;

	    private_bgcolor = -1;
	    private_czmask = -1;
	    if (botbatch.pr) {
		private_fgcolor = cp->back;
		pr_batchrop(PixRect, ms->x, ms->y,
			    PIX_SRC | PIX_DST,
			    &botbatch, 1);
	    }
	    private_fgcolor = cp->fore;
	    if (topbatch.pr)
		pr_batchrop(PixRect, ms->x, ms->y,
			    PIX_NOT(PIX_SRC) & PIX_DST,
			    &topbatch, 1);
	    private_czmask = old_zmask;
	}
	CursorDisplayed = 1;
    }
}

int
CheckCursor(x, y, w, h)
{
    register vsCursor *ms = CurrentDevice->mouse;
    register CURSOR *cs = CurrentCursor;

    if (CursorDisplayed
	&& OverLap(x, y, w, h, ms->x, ms->y, cs->width, cs->height)) {
	    DisplayCursor(NULL);
	}
}

RestoreCursor()
{
    if (!CursorDisplayed)
	DisplayCursor(CurrentCursor);
}
#endif	sun
