    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

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

#include "Xapollo.h"

extern char *Xalloc();
extern DEVICE *CurrentDevice;

CURSOR *CurrentCursor;
int CursorDisplayed;

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
    cursor->xmin = 0;
    cursor->ymin = 0;
    cursor->xoff = xoff;
    cursor->yoff = yoff;
    cursor->xmax = Screen.width - image->width;
    cursor->ymax = Screen.height - image->height;
    cursor->refcnt = 1;

    data = (CursPriv *) Xalloc(sizeof(CursPriv));
    cursor->data = (caddr_t) data;
    image->refcnt++;
    data->bits = (caddr_t) image;
    data->fore = fore;
    data->back = back;
    data->save = make_bitmap(NULL, image->width, image->height+2, false);
    if (mask) {
    	data->mask = mask;
    	mask->refcnt++;
    } else
    	data->mask = NULL;
    return (cursor);
}

FreeCursor (cursor)
	register CURSOR *cursor;
{
    CursPriv   *cp = CDATA(cursor);

    if (--(cp->bits->refcnt) <= 0)
    	FreeBitmap(cp->bits);
    if (cp->mask && --(cp->mask->refcnt) <= 0)
    	FreeBitmap(cp->mask);
    FreeBitmap(cp->save);
    free((caddr_t) CDATA(cursor));
    free((caddr_t) cursor);
}


LoadCursor (cursor)
	register CURSOR *cursor;
{
    if (CursorDisplayed)
    	DisplayCursor(NULL);
    if ((CurrentCursor = cursor) != NULL) {
    	DisplayCursor(cursor);
        }
}

InitMouse ()
{
}

SetCursorPosition(pos)
	register vsCursor *pos;
{
    if (pos->x != (CurrentDevice->mouse->x) ||
        pos->y != (CurrentDevice->mouse->y)) {
    	if (CursorDisplayed)
	    DisplayCursor(NULL);
    	CurrentDevice->mouse->x = pos->x;
	    CurrentDevice->mouse->y = pos->y;
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
    vsCursor   *ms = CurrentDevice->mouse;
    short op, i, msx, msy;
    gpr_$window_t srcwin;
    gpr_$position_t dstorg, destorg;
    status_$t status;
    static gpr_$attribute_desc_t ab = -1;
    gpr_$window_t saved_clip_wind;
    gpr_$window_t screen_clip_wind;
    boolean saved_clip_active;
    gpr_$mask_t saved_plane_mask;
    gpr_$raster_op_array_t saved_rops;
    

/* Use a private attribute block, since caller has probably already set
   lots of attributes
  --   We disable this, and just save/restore raster ops, clip window and plane mask,
  --   because attribute block switching (1) is expensive, especially on color hardware, and
  --   (2) screws up input enables something awful.
 */
/*
    if (ab == -1)  {
        gpr_$allocate_attribute_block( ab, status );
        gpr_$set_attribute_block( ab, status );
        gpr_$set_plane_mask(Screen.plane_mask, status);
        }
    gpr_$set_attribute_block( ab, status );
*/
    gpr_$inq_raster_ops (saved_rops, status);
    gpr_$inq_constraints (saved_clip_wind, saved_clip_active, saved_plane_mask, status);
    screen_clip_wind.window_base.x_coord = 0;
    screen_clip_wind.window_base.y_coord = 0;
    screen_clip_wind.window_size.x_size = Screen.width;
    screen_clip_wind.window_size.y_size = Screen.height;
    gpr_$set_clip_window(screen_clip_wind, status);
    gpr_$set_plane_mask( Screen.plane_mask, status);

    msx = ms->x < 0 ? 0 : ms->x;
    msy = ms->y < 0 ? 0 : ms->y;
    dstorg.x_coord = msx;
    dstorg.y_coord = msy;
    if (cs == NULL) {
	if (CursorDisplayed && (cs = CurrentCursor) != NULL) {
	    /* take it out */  
        srcwin.x_coord = srcwin.y_coord = 0;
        srcwin.x_size = cs->width;
        srcwin.y_size = cs->height;
        for (i=0; i<Screen.depth; i++) 
            gpr_$set_raster_op( (gpr_$plane_t)i, (gpr_$raster_op_t)3, status );
        gpr_$pixel_blt(CDATA(cs)->save->data, srcwin, dstorg, status);
	    CursorDisplayed = 0;
        }
    }
    else {
	    int botop = (CDATA(cs)->back == 1 ? 7 : 4);
	    int topop = (CDATA(cs)->fore == 1 ? 7 : 4);

        CursorDisplayed = -1;

        /* save image beneath cursor */
        srcwin.x_size = cs->width;
        srcwin.y_size = cs->height;
        srcwin.x_coord = msx;
        srcwin.y_coord = msy;
        destorg.x_coord = destorg.y_coord = 0;
        gpr_$set_bitmap(CDATA(cs)->save->data, status);
/*  the following code is redundant
        for (i=0; i<Screen.depth; i++) 
            gpr_$set_raster_op( (gpr_$plane_t)i, (gpr_$raster_op_t)3, status );
*/
        gpr_$pixel_blt(Screen.bm, srcwin, destorg, status);
        gpr_$set_bitmap(Screen.bm, status);

        /* display mask */
        srcwin.x_coord = srcwin.y_coord = 0;
        if (CDATA(cs)->mask != NULL) {
            for (i=0; i<Screen.depth; i++) 
                gpr_$set_raster_op( (gpr_$plane_t)i, (gpr_$raster_op_t)botop, status );
            gpr_$pixel_blt(CDATA(cs)->mask->data, srcwin, dstorg, status);
            }
        /* display cursor itself */
        for (i=0; i<Screen.depth; i++) 
            gpr_$set_raster_op( (gpr_$plane_t)i, (gpr_$raster_op_t)topop, status );
        gpr_$pixel_blt(CDATA(cs)->bits->data, srcwin, dstorg, status);
        
        }
/*
    gpr_$set_attribute_block( Screen.ab, status );
*/
    for (i=0; i<Screen.depth; i++) 
        gpr_$set_raster_op( (gpr_$plane_t)i, saved_rops[i], status );
    gpr_$set_clip_window(saved_clip_wind, status);
    gpr_$set_plane_mask(saved_plane_mask, status);
}
