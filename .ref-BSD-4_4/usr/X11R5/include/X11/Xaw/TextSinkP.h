/*
* $XConsortium: TextSinkP.h,v 1.4 90/04/30 17:46:39 converse Exp $
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
 * TextSinkP.h - Private definitions for TextSink object
 * 
 */

#ifndef _XawTextSinkP_h
#define _XawTextSinkP_h

/***********************************************************************
 *
 * TextSink Object Private Data
 *
 ***********************************************************************/

#include <X11/ObjectP.h>
#include <X11/Xaw/TextP.h>	/* This source works with the Text widget. */
#include <X11/Xaw/TextSrcP.h>	/* This source works with the Text Source. */
#include <X11/Xaw/TextSink.h>

/************************************************************
 *
 * New fields for the TextSink object class record.
 *
 ************************************************************/

typedef struct _TextSinkClassPart {
  void (*DisplayText)();
  void (*InsertCursor)();
  void (*ClearToBackground)();
  void (*FindPosition)();
  void (*FindDistance)();
  void (*Resolve)();
  int  (*MaxLines)();
  int  (*MaxHeight)();
  void (*SetTabs)();		
  void (*GetCursorBounds)();	
} TextSinkClassPart;

/* Full class record declaration */
typedef struct _TextSinkClassRec {
    ObjectClassPart     object_class;
    TextSinkClassPart	text_sink_class;
} TextSinkClassRec;

extern TextSinkClassRec textSinkClassRec;

/* New fields for the TextSink object record */
typedef struct {
    /* resources */
    XFontStruct	*font;		/* Font to draw in. */
    Pixel foreground;		/* Foreground color. */
    Pixel background;		/* Background color. */

    /* private state. */
    Position *tabs;		/* The tab stops as pixel values. */
    short    *char_tabs;	/* The tabs stops as character values. */
    int      tab_count;		/* number of items in tabs */

} TextSinkPart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _TextSinkRec {
  ObjectPart    object;
  TextSinkPart	text_sink;
} TextSinkRec;

/************************************************************
 *
 * Private declarations.
 *
 ************************************************************/

typedef int (*_XawSinkIntFunc)();
typedef void (*_XawSinkVoidFunc)();

#define XtInheritDisplayText          ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritInsertCursor         ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritClearToBackground    ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritFindPosition         ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritFindDistance         ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritResolve              ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritMaxLines             ((_XawSinkIntFunc) _XtInherit)
#define XtInheritMaxHeight            ((_XawSinkIntFunc) _XtInherit)
#define XtInheritSetTabs              ((_XawSinkVoidFunc) _XtInherit)
#define XtInheritGetCursorBounds      ((_XawSinkVoidFunc) _XtInherit)

#endif /* _XawTextSinkP_h */
