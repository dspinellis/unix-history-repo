#ifndef lint
static char *rcsid_cursor_c = "$Header: cursor.c,v 10.3 86/12/17 18:05:41 swick Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* cursor.c - various stuff with the mouse & cursor
 *
 *	StoreCursor		Creates a cursor
 *	FreeCursor		Frees the storage taken by a cursor
 *	LoadCursor		Loads a bitmap to use as cursor
 *	InitMouse		Initialize the mouse
 *	SetCursorPosition	Forces cursor to a particular position
 *	SetMouseCharacteristics	Controls speed of cursor relative to mouse
 *
 *  	Changes and additions by:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"

#ifdef APA16
#include "apa16.h"
#endif APA16

#ifdef AED
#include "aed.h"
#endif AED

#ifdef APA8
#include "apa8.h"
#endif APA8

#ifdef APA8C
#include "apa8c.h"
#endif APA8C

#ifdef PQD
#include "pqd.h"
#endif PQD

#define imin(a,b) ((a) < (b) ? (a) : (b))

/*
 * Create a cursor 
 */

/*ARGSUSED*/
CURSOR *StoreCursor (func, image, fore, back, mask, xoff, yoff)
	register BITMAP *image;
	BITMAP *mask;
	int func, fore, back, xoff, yoff;
{
	register CURSOR *cursor;
	register u_short *cimage = (u_short *)image->data;
	register u_short *cmask = (u_short *)mask->data;
	register CursPriv *curdata;
	register i, width_mask;
	static u_short bmask[] = { 0x0000, 0x8000, 0xC000, 0xE000,
				   0xF000, 0xF800, 0xFC00, 0xFE00,
				   0xFF00, 0xFF80, 0xFFC0, 0xFFE0,
				   0xFFF0, 0xFFF8, 0xFFFC, 0xFFFE, 0xFFFF };

	static u_short defmask[] = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
				     0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
				     0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
				     0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF };

#ifdef TRACE_X
	fprintf(stderr, "In StoreCursor\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * If no mask supplied use default 
	 */

	if (mask == NULL)
		cmask = defmask;

	/*
	 * Allocate space for cursor structure
	 */

	cursor = (CURSOR *) Xalloc (sizeof (CURSOR));

	/*
	 * Fill in cursor structure
	 */

	cursor->width = imin(image->width, CURSOR_WIDTH);
	cursor->height = imin(image->height, CURSOR_HEIGHT);
	cursor->xmin = 0;
	cursor->ymin = 0;
	cursor->xmax = pbm.width - 1;
	cursor->ymax = pbm.height - 1;
	cursor->xoff = xoff;
	cursor->yoff = yoff;
	cursor->refcnt = 1;

	/*
	 * Allocate space for CursPriv structure which holds
	 * cursor image and mask
	 */

	curdata = (CursPriv *) Xalloc (sizeof (CursPriv));
	cursor->data = (caddr_t) curdata;

	/*
	 * Get width mask
	 *
	 * Note: cursor image and mask are in IBM bit order
	 *       not VAX. Therefore, width masks are in IBM
	 *       bit order.
	 */

	width_mask = bmask[cursor->width];
	
	if (fore == 0) {
		/*
		 * Invert and store cursor in CursPriv structure
		 */

		for ( i = 0; i < cursor->height; i++) {
			curdata->mask[i] = (*cmask++) & width_mask;
			curdata->data[i] = ((*cimage++) & width_mask)
						^ curdata->mask[i];
		}
	} else	{
		/*
		 * Store cursor in CursPriv structure
		 */

		for ( i = 0; i < cursor->height; i++) {
			curdata->data[i] = (*cimage++) & width_mask;
			curdata->mask[i] = (*cmask++) & width_mask;
		}
	}

	/*
	 * Pad cursor image and mask to maximum cursor size
	 */

	for (i = cursor->height; i < CURSOR_HEIGHT; i++) {
		curdata->data[i] = 0;
		curdata->mask[i] = 0;
	}

	/*
	 * Set cursor hotspot
	 */

	curdata->hotspot.x = curdata->hotspot.y = 0;

	/*
	 * Return pointer to cursor structure
	 */

	return (cursor);
}

/*
 * Free resources held by cursor
 */

FreeCursor (cursor)
	register CURSOR *cursor;
{
#ifdef TRACE_X
	fprintf(stderr, "In FreeCursor\n");
	fflush(stderr);
#endif TRACE_X

	free ((caddr_t) CDATA(cursor));
	free ((caddr_t) cursor);
}

/*
 * Set hotspot and load new cursor
 */

LoadCursor (cursor)
	register CURSOR *cursor;
{
#ifdef TRACE_X
	fprintf(stderr, "In LoadCursor\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Set new hotspot
	 */

	XAddr->hotspot.x = cursor->xoff;
	XAddr->hotspot.y = cursor->yoff;

	/*
	 * Load new cursor
	 */

	return(ioctl (xdev, QIOCLDCUR, (caddr_t) cursor->data));
}

/*
 * Initialize mouse 
 */

InitMouse ()
{
	char *MouseType = getenv("MOUSETYPE");
	char *MouseName = getenv("MOUSENAME");
	struct sgttyb MouseSG;
	int ldisc, ioarg;
	extern int mdev;

#ifdef TRACE_X
	fprintf(stderr, "In InitMouse\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Switch to mouse line discipline
	 */

	ldisc = TABLDISC;
	ioctl(mdev, TIOCSETD, (caddr_t) &ldisc);

	/*
	 * Check for Mouse Systems Mouse
	 */

	if (MouseType && (strcmp (MouseType, "MSCMOUSE") == 0)) {
		MouseSG.sg_ispeed = 9;
		MouseSG.sg_ospeed = 9;
		MouseSG.sg_erase = -1;
		MouseSG.sg_kill = -1;
		MouseSG.sg_flags = RAW | ANYP;
		ioctl(mdev, TIOCSETP, (caddr_t) &MouseSG);

		/*
		 * Set to MSCmouse emulation
		 */

		ldisc = PCMS_DISC;
		if (ioctl (mdev, TBIOSETD, (caddr_t) &ldisc) < 0)
		    DeviceError("Error in setting PCMS_DISC Line Discipline");

	} else {	/* Assume the Planar Mouse */

		/*
		 * Use 3 button emulation
		 */

		ldisc = PLANMS_DISC3;
		if (ioctl (mdev, TBIOSETD, (caddr_t) &ldisc) < 0)
		   DeviceError("Error in setting PLANMS_DISC3 Line Discipline");

		/*
		 * Set defualt mouse sample rate
		 */

		ioarg = MS_RATE_40;
		if (ioctl (mdev, MSIC_SAMP, (caddr_t) &ioarg) < 0)
			DeviceError ("Error in setting mouse sample rate");

		/*
		 * Set defualt mouse resolution
		 */

		ioarg = MS_RES_200;
		if (ioctl (mdev, MSIC_RESL, (caddr_t) &ioarg) < 0)
			DeviceError ("Error in setting mouse resolution");
	}
	 
}

/*
 * Set new cursor position
 */

SetCursorPosition(pos)
	register XCursor *pos;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetCursorPosition\n");
	fflush(stderr);
#endif TRACE_X

	return(ioctl(xdev, QIOCSMSTATE, (caddr_t) pos));
}

/*
 * Set mouse threshold and acceleration (mouse speed control)
 */

SetMouseCharacteristics (threshold, accelaration)
	int threshold, accelaration;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetMouseCharacteristics\n");
	fflush(stderr);
#endif TRACE_X

	XAddr->mscale = accelaration;
	XAddr->mthreshold = threshold;
}
