/* Copyright 1985 Massachusetts Institute of Technology */

/* ddxcursor.c	various stuff with the mouse & cursor
 *
 *	StoreCursor		Creates a cursor
 *	FreeCursor		Frees the storage taken by a cursor
 *	LoadCursor		Loads a bitmap to use as cursor
 *	InitMouse		Initialize the mouse
 *	SetCursorPosition	Forces cursor to a particular position
 *	SetMouseCharacteristics	Controls speed of cursor relative to mouse
 *
 */

#include "ddxqvss.h"
#include <vaxuba/qvioctl.h>
#include "qvss.h"

extern vsIoAddr *VSAddr;
extern BITMAP vbm;
extern int vsdev;

char *Xalloc();
/* someday this routine needs to be thought through and redone */

CURSOR *StoreCursor (func, image, fore, back, mask, xoff, yoff)
	register BITMAP *image;
	BITMAP *mask;
	int func, fore, back, xoff, yoff;
{
	register CURSOR *cursor;
	register short *im = (short *)image->data;
	register short *cmask = (short *)mask->data;
	register CursPriv *data;
	register int i, wmsk;
	extern char FBMap[];
	static short bmask[] = {   0x0000, 0x0001, 0x0003, 0x0007,
				   0x000f, 0x001f, 0x003f, 0x007f,
				   0x00ff, 0x01ff, 0x03ff, 0x07ff,
				   0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff };

	static short defmask[] = { 0xffff, 0xffff, 0xffff, 0xffff,
				   0xffff, 0xffff, 0xffff, 0xffff,
				   0xffff, 0xffff, 0xffff, 0xffff,
				   0xffff, 0xffff, 0xffff, 0xffff };

	if (mask == NULL) cmask = defmask;

	cursor = (CURSOR *) Xalloc (sizeof (CURSOR));
	cursor->width = min(image->width, CURSOR_WIDTH);
	cursor->height = min(image->height, CURSOR_HEIGHT);
	cursor->xmin = xoff;
	cursor->ymin = yoff;
	cursor->xoff = xoff;
	cursor->yoff = yoff;
	cursor->xmax = vbm.width - (image->width - xoff);
	cursor->ymax = vbm.height - (image->height - yoff);
	cursor->refcnt = 1;
	data = (CursPriv *) Xalloc (sizeof (CursPriv));
	cursor->data = (caddr_t) data;
	if (fore & 1)
	    func += 0x20;
	if (back & 1)
	    func += 0x10;
	func = FBMap[func];

	wmsk = bmask[cursor->width];
	
	if (fore == 0 || func == GXxor || func == GXequiv) {
		for ( i = 0; i < cursor->height; i++)
			data->cbits[i] = (*im++ & *cmask++) & wmsk;
	}
	else {
		for ( i = 0; i < cursor->height; i++)
			data->cbits[i] = (~*im++ & *cmask++) & wmsk;
	}
		
	for (i = cursor->height; i < 16; i++);
		data->cbits[i] = 0;
	return (cursor);
}

FreeCursor (cursor)
	register CURSOR *cursor;
{
	free ((caddr_t) CDATA(cursor));
	free ((caddr_t) cursor);
}

LoadCursor (cursor)
	register CURSOR *cursor;
{
	register short *cbits = VSAddr->cursorbits;
	register short *nbits;
	register int i;
	int nrows = cursor->height;
	int ncols = cursor->width;

	nbits = (short *) CDATA(cursor)->cbits;
	if (ncols > 16) ncols = 16;
	if (nrows > 16) nrows = 16;
	
	for(i = 0; i < nrows; i++) 
		*cbits++ = *nbits++;

	for(i = nrows; i < 16; i++)
		*cbits++ = 0;
	return(0);
}

InitMouse ()
{
}

SetCursorPosition(pos)
	register vsCursor *pos;
{
	return(ioctl(vsdev, QIOCSMSTATE,pos));
}

SetMouseCharacteristics (threshold, accelaration)
	int threshold, accelaration;
{
	VSAddr->mscale = accelaration;
	VSAddr->mthreshold = threshold;
}
