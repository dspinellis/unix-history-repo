/*
 *	$Source: /u1/X/libis/RCS/cursor.c,v $
 *	$Header: cursor.c,v 1.1 86/11/17 14:33:26 swick Rel $
 */

#ifndef lint
static char *rcsid_cursor_c = "$Header: cursor.c,v 1.1 86/11/17 14:33:26 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	cursor.c - various stuff with the mouse & cursor
 *
 *	StoreCursor		Creates a cursor
 *	FreeCursor		Frees the storage taken by a cursor
 *	LoadCursor		Loads a bitmap to use as cursor
 *	InitMouse		Initialize the mouse
 *	SetCursorPosition	Forces cursor to a particular position
 *	UpdateCursorPosition	Moves cursor to a particular position
 *	SetMouseCharacteristics	Controls speed of cursor relative to mouse
 *
 *      Copyright (c) 1986, Integrated Solutions, Inc.
 *
 */

#include "Xis.h"

static CURSOR	*CurrentCursor;
static int	CursorDisplayed;

extern BITMAP	*StoreBitmap();
extern PIXMAP	*MakePixmap();
extern DEVICE	*CurrentDevice;

/*
 *	StoreCursor
 */
CURSOR *StoreCursor(func, image, fore, back, mask, xoff, yoff)
int		func;
register BITMAP	*image;
register int	fore, back;
BITMAP		*mask;
int		xoff, yoff;
{
    register CURSOR	*cursor;
    register CursPriv	*data;
    BITMAP		*bitmap;

#ifdef DEBUG
if (debug & D_Cursor)
    printf("StoreCursor(func=%d, image=0x%x, fore=%d, back=%d, mask=0x%x,\n	xoff=%d, yoff=%d)\n",
	func, image, fore, back, mask, xoff, yoff);
#endif DEBUG

    if (!image) {
	return (NULL);
    }

    cursor = (CURSOR *) Xalloc(sizeof(CURSOR));

    cursor->width	= image->width;
    cursor->height	= image->height;
    cursor->xoff	= xoff;
    cursor->yoff	= yoff;

    /* Only the tip need be on-screen */
    cursor->xmin	= 0;
    cursor->ymin	= 0;
    cursor->xmax	= CurrentDevice->width;
    cursor->ymax	= CurrentDevice->height;

    cursor->refcnt	= 1;

    data = (CursPriv *) Xalloc(sizeof(CursPriv));

    cursor->data = (caddr_t) data;

    data->image		= MakePixmap(image, fore, back);
    data->image->tile	= CannotBeTiled;
    data->mask		= mask;
    if (mask) {
	mask->refcnt++;
    }
    data->func		= func;
    data->fore		= fore;
    data->back		= back;

    bitmap = StoreBitmap(image->width, image->height, (char *)NULL);
    data->save = MakePixmap(bitmap, fore, back);
    data->save->tile	= CannotBeTiled;
    return (cursor);
}

/*
 *	FreeCursor
 */
FreeCursor(cursor)
CURSOR	*cursor;
{
    register CursPriv *cp = CDATA(cursor);

#ifdef DEBUG
if (debug & D_Cursor)
    printf("FreeCursor(cursor=0x%x)\n", cursor);
#endif DEBUG

    FreePixmap(cp->image);
    if (cp->mask) {
	FreeBitmap(cp->mask);
    }
    FreePixmap(cp->save);
    free((caddr_t) cp);
    free((caddr_t) cursor);
}

/*
 *	LoadCursor
 */
LoadCursor(cursor)
register CURSOR	*cursor;
{

#ifdef DEBUG
if (debug & D_Cursor)
    printf("LoadCursor(cursor=0x%x)\n", cursor);
#endif DEBUG

    if (CurrentCursor != cursor) {
	if (CursorDisplayed) {
	    DisplayCursor((CURSOR *)NULL);
	}
	if ((CurrentCursor = cursor) != NULL) {
	    DisplayCursor(cursor);
	}
    }
}

/*
 *	InitMouse
 */
static short sbounds[4];
InitMouse()
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("InitMouse()\n");
#endif DEBUG
    sbounds[0] = 0;
    sbounds[1] = ScreenPixmap.width;
    sbounds[2] = 0;
    sbounds[3] = ScreenPixmap.height;
}


/*
 *	SetCursorPosition
 */
SetCursorPosition(pos)
register vsCursor *pos;
{
    extern int indev, invalid_mouse;

#ifdef DEBUG
if (debug & D_Cursor)
    printf("SetCursorPosition(pos->x=%d, pos->y=%d)\n", pos->x, pos->y);
#endif DEBUG

    if (pos->x != (CurrentDevice->mouse->x) ||
	pos->y != (CurrentDevice->mouse->y)) {
	/* keep mouse in sync with cursor */
	short mbounds[4];
	mbounds[0] = mbounds[1] = pos->x;
	mbounds[2] = mbounds[3] = pos->y;
	ioctl(indev, TIOUMBND, mbounds);	/* warps mouse cursor */
	ioctl(indev, TIOUMBND, sbounds);	/* sets bounds back */
	invalid_mouse = 1;
	UpdateCursorPosition(pos);
    }
}

vsCursor last_mouse;	/* last known mouse position */

/*
 *	UpdateCursorPosition
 */
UpdateCursorPosition(pos)
register vsCursor *pos;
{

#ifdef DEBUG
if (debug & D_Cursor)
    printf("UpdateCursorPosition(pos->x=%d, pos->y=%d)\n", pos->x, pos->y);
#endif DEBUG

    /* assumes mouse is in sync with cursor */
    if (pos->x != (CurrentDevice->mouse->x) ||
	pos->y != (CurrentDevice->mouse->y)) {
	if (CursorDisplayed) {
	    DisplayCursor((CURSOR *)NULL);
	}
	*(CurrentDevice->mouse) = *pos;
	last_mouse = *pos;		/* update last mouse position */
	DisplayCursor(CurrentCursor);
    }
}

SetMouseCharacteristics(threshold, acceleration)
int	threshold, acceleration;
{
    extern int mouse_threshold, mouse_acceleration;
#ifdef DEBUG
if (debug & D_Misc)
    printf("SetMouseCharacteristics(threshold=%d, acceleration=%d)\n",
	threshold, acceleration);
#endif DEBUG
    mouse_threshold = threshold;
    mouse_acceleration = acceleration;
}

/*
 *	DisplayCursor
 */
static DisplayCursor(cursor)
CURSOR	*cursor;
{
    register vsCursor *ms = CurrentDevice->mouse;
    register CursPriv *cp = CDATA(cursor);
    register int x = ms->x;
    register int y = ms->y;
    CLIP	i;
    CLIP	bc, bs;

#ifdef DEBUG
if (debug & D_Cursor)
    printf("DisplayCursor(cursor=0x%x)\n", cursor);
#endif DEBUG

    bs.left = 0;
    bs.width = ScreenPixmap.width;
    bs.top = 0;
    bs.height = ScreenPixmap.height;

    if (cursor == NULL) {
	if (CurrentCursor) {
	    /* pick up cursor (put background back) */
	    cp = CDATA(CurrentCursor);
	    bc.left = x;
	    bc.width = CurrentCursor->width;
	    bc.top = y;
	    bc.height = CurrentCursor->height;
	    i = Intersection(bc, bs);

	    GIP_RasterOp(GIPcopy,
		cp->save, i.left-x, i.top-y,
		&ScreenPixmap, i.left, i.top,
		(BITMAP *)NULL, 0, 0,
		i.width, i.height,
		~0);

	    CursorDisplayed = 0;
	}
    } else {
	bc.left = x;
	bc.width = cursor->width;
	bc.top = y;
	bc.height = cursor->height;
	i = Intersection(bc, bs);

	/* save background */
	GIP_RasterOp(GIPcopy,
	    &ScreenPixmap, i.left, i.top,
	    cp->save, i.left-x, i.top-y,
	    (BITMAP *)NULL, 0, 0,
	    i.width, i.height,
	    ~0);

	/* put cursor down */
	GIP_RasterOp((unsigned char)(cp->func),
	    cp->image, i.left-x, i.top-y,
	    &ScreenPixmap, i.left, i.top,
	    cp->mask, i.left-x, i.top-y,
	    i.width, i.height,
	    ~0);

	CursorDisplayed = 1;
    }
}

/*
 *	CheckCursor
 */
CheckCursor(r1)
CLIP	r1;
{
    register vsCursor *ms = CurrentDevice->mouse;
    register CURSOR *cursor = CurrentCursor;

    CLIP r2;

#ifdef DEBUG
if (debug & D_Cursor) {
    printf("CheckCursor()\n");
    printf_clip("	bounds", r1);
}
#endif DEBUG

    if (cursor == NULL) {
	return;
    }

    r2.left = ms->x;
    r2.top = ms->y;
    r2.width = cursor->width;
    r2.height = cursor->height;

#ifdef DEBUG
if (debug & D_Cursor)
    printf_clip("	cursor", r2);
#endif DEBUG

    if (CursorDisplayed && Overlap(r1, r2)) {
	DisplayCursor((CURSOR *)NULL);
    }
}

/*
 *	RestoreCursor
 */
RestoreCursor()
{

#ifdef DEBUG
if (debug & D_Cursor)
    printf("RestoreCursor()\n");
#endif DEBUG

    if (!CursorDisplayed) {
	DisplayCursor(CurrentCursor);
    }
}
