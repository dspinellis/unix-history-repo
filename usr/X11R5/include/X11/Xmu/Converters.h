/* $XConsortium: Converters.h,v 1.14 91/07/22 23:51:43 converse Exp $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 * The interfaces described by this header file are for miscellaneous utilities
 * and are not part of the Xlib standard.
 */

#ifndef _XMU_STRCONVERT_H_
#define _XMU_STRCONVERT_H_

#include <X11/Xfuncproto.h>

_XFUNCPROTOBEGIN

/*
 * Converters - insert in alphabetical order
 */

/******************************************************************************
 * XmuCvtFunctionToCallback
 */
extern void XmuCvtFunctionToCallback(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToBackingStore
 */
#define XtNbackingStore "backingStore"
#define XtCBackingStore "BackingStore"
#define XtRBackingStore "BackingStore"
#define XtEnotUseful "notUseful"
#define XtEwhenMapped "whenMapped"
#define XtEalways "always"
#define XtEdefault "default"
extern void XmuCvtStringToBackingStore(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToCursor
 */
extern void XmuCvtStringToCursor(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);
extern Boolean XmuCvtStringToColorCursor(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */,
    XtPointer*		/* converter_data */
#endif
);

#define XtRColorCursor "ColorCursor"
#define XtNpointerColor "pointerColor"
#define XtNpointerColorBackground "pointerColorBackground"


/******************************************************************************
 * XmuCvtStringToGravity
 */
typedef int XtGravity;

#define XtRGravity "Gravity"
#define XtEForget "forget"
#define XtENorthWest "northwest"
#define XtENorth "north"
#define XtENorthEast "northeast"
#define XtEWest "west"
#define XtECenter "center"
#define XtEEast "east"
#define XtESouthWest "southwest"
#define XtESouth "south"
#define XtESouthEast "southeast"
#define XtEStatic "static"
#define XtEUnmap "unmap"

extern void XmuCvtStringToGravity (
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToJustify
 */
typedef enum {
    XtJustifyLeft,       /* justify text to left side of button   */
    XtJustifyCenter,     /* justify text in center of button      */
    XtJustifyRight       /* justify text to right side of button  */
} XtJustify;
#define XtEleft "left"
#define XtEcenter "center"
#define XtEright "right"
#define XtEtop "top"
#define XtEbottom "bottom"

extern void XmuCvtStringToJustify(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToLong
 */
#define XtRLong "Long"
extern void XmuCvtStringToLong(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToOrientation
 */
typedef enum {XtorientHorizontal, XtorientVertical} XtOrientation;
extern void XmuCvtStringToOrientation(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToBitmap
 */
extern void XmuCvtStringToBitmap(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToShapeStyle; is XtTypeConverter (i.e. new style)
 * no conversion arguments, not particularly useful to cache the results.
 */

#define XtRShapeStyle "ShapeStyle"
#define XtERectangle "Rectangle"
#define XtEOval "Oval"
#define XtEEllipse "Ellipse"
#define XtERoundedRectangle "RoundedRectangle"

#define XmuShapeRectangle 1
#define XmuShapeOval 2
#define XmuShapeEllipse 3
#define XmuShapeRoundedRectangle 4

extern Boolean XmuCvtStringToShapeStyle(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */,
    XtPointer*		/* converter_data */
#endif
);

extern Boolean XmuReshapeWidget(
#if NeedFunctionPrototypes
    Widget	/* w */,
    int		/* shape_style */,
    int		/* corner_width */,
    int		/* corner_height */
#endif
);

/******************************************************************************
 * XmuCvtStringToWidget
 */
extern void XmuCvtStringToWidget(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);

/******************************************************************************
 * XmuNewCvtStringToWidget
 */
extern Boolean XmuNewCvtStringToWidget(
#if NeedFunctionPrototypes
    Display*            /* display */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValue*		/* fromVal */,
    XrmValue*		/* toVal */,
    XtPointer*          /* converter_data */
#endif
);

_XFUNCPROTOEND

#endif /* _XMU_STRCONVERT_H_ */
