/*
* $XConsortium: BoxP.h,v 1.16 89/11/06 10:51:28 swick Exp $
*/


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
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
 * BoxP.h - Private definitions for Box widget
 * 
 */

#ifndef _XawBoxP_h
#define _XawBoxP_h

/***********************************************************************
 *
 * Box Widget Private Data
 *
 ***********************************************************************/

#include <X11/Xaw/Box.h>
#include <X11/CompositeP.h>
#include <X11/Xmu/Converters.h>

/* New fields for the Box widget class record */
typedef struct {int empty;} BoxClassPart;

/* Full class record declaration */
typedef struct _BoxClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    BoxClassPart	box_class;
} BoxClassRec;

extern BoxClassRec boxClassRec;

/* New fields for the Box widget record */
typedef struct {
    /* resources */
    Dimension   h_space, v_space;
    XtOrientation orientation;

    /* private state */
    Dimension	preferred_width, preferred_height;
    Dimension	last_query_width, last_query_height;
    XtGeometryMask last_query_mode;
} BoxPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _BoxRec {
    CorePart	    core;
    CompositePart   composite;
    BoxPart 	    box;
} BoxRec;

#endif /* _XawBoxP_h */
