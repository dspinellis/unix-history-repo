#ifndef _XATOMTYPE_H_
#define _XATOMTYPE_H_

/* $XConsortium: Xatomtype.h,v 11.14 89/04/19 15:09:29 jim Exp $ */

/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * This files defines crock C structures for calling XGetWindowProperty and
 * XChangeProperty.  All fields must be longs as the semantics of property
 * routines will handle conversion to and from actual 32 bit objects.  If your
 * compiler doesn't treat &structoflongs the same as &arrayoflongs[0], you
 * will have some work to do.
 */

#define BOOL long
#define SIGNEDINT long
#define UNSIGNEDINT unsigned long
#define RESOURCEID unsigned long


/* this structure may be extended, but do not change the order */
typedef struct {
    UNSIGNEDINT flags;
    SIGNEDINT x, y, width, height;	/* need to cvt; only for pre-ICCCM */
    SIGNEDINT minWidth, minHeight;	/* need to cvt */
    SIGNEDINT maxWidth, maxHeight;	/* need to cvt */
    SIGNEDINT widthInc, heightInc;	/* need to cvt */
    SIGNEDINT minAspectX, minAspectY;	/* need to cvt */
    SIGNEDINT maxAspectX, maxAspectY;	/* need to cvt */
    SIGNEDINT baseWidth,baseHeight;	/* need to cvt; ICCCM version 1 */
    SIGNEDINT winGravity;		/* need to cvt; ICCCM version 1 */
} xPropSizeHints;
#define OldNumPropSizeElements 15	/* pre-ICCCM */
#define NumPropSizeElements 18		/* ICCCM version 1 */

/* this structure may be extended, but do not change the order */
/* RGB properties */
typedef struct {
	RESOURCEID colormap;
	UNSIGNEDINT red_max;
	UNSIGNEDINT red_mult;
	UNSIGNEDINT green_max;
	UNSIGNEDINT green_mult;
	UNSIGNEDINT blue_max;
	UNSIGNEDINT blue_mult;
	UNSIGNEDINT base_pixel;
	RESOURCEID visualid;		/* ICCCM version 1 */
	RESOURCEID killid;		/* ICCCM version 1 */
} xPropStandardColormap;
#define OldNumPropStandardColormapElements 8  /* pre-ICCCM */
#define NumPropStandardColormapElements 10  /* ICCCM version 1 */


/* this structure may be extended, but do not change the order */
typedef struct {
    UNSIGNEDINT flags;
    BOOL input;				/* need to convert */
    SIGNEDINT initialState;		/* need to cvt */
    RESOURCEID iconPixmap;
    RESOURCEID iconWindow;
    SIGNEDINT  iconX;			/* need to cvt */
    SIGNEDINT  iconY;			/* need to cvt */
    RESOURCEID iconMask;
    UNSIGNEDINT windowGroup;
  } xPropWMHints;
#define NumPropWMHintsElements 9 /* number of elements in this structure */

/* this structure defines the icon size hints information */
typedef struct {
    SIGNEDINT minWidth, minHeight;	/* need to cvt */
    SIGNEDINT maxWidth, maxHeight;	/* need to cvt */
    SIGNEDINT widthInc, heightInc;	/* need to cvt */
  } xPropIconSize;
#define NumPropIconSizeElements 6 /* number of elements in this structure */

/* this structure defines the window manager state information */
typedef struct {
    SIGNEDINT state;			/* need to cvt */
    RESOURCEID iconWindow;
} xPropWMState;
#define NumPropWMStateElements 2	/* number of elements in struct */

#undef BOOL
#undef SIGNEDINT
#undef UNSIGNEDINT
#undef RESOURCEID

#endif /* _XATOMTYPE_H_ */
