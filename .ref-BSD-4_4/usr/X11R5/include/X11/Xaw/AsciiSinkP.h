/*
* $XConsortium: AsciiSinkP.h,v 1.2 89/10/04 13:56:34 kit Exp $
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
 * asciiSinkP.h - Private definitions for asciiSink object
 * 
 */

#ifndef _XawAsciiSinkP_h
#define _XawAsciiSinkP_h

/***********************************************************************
 *
 * AsciiSink Object Private Data
 *
 ***********************************************************************/

#include <X11/Xaw/TextSinkP.h> 
#include <X11/Xaw/AsciiSink.h> 

/************************************************************
 *
 * New fields for the AsciiSink object class record.
 *
 ************************************************************/

typedef struct _AsciiSinkClassPart {
  int foo;
} AsciiSinkClassPart;

/* Full class record declaration */

typedef struct _AsciiSinkClassRec {
    ObjectClassPart     object_class;
    TextSinkClassPart	text_sink_class;
    AsciiSinkClassPart	ascii_sink_class;
} AsciiSinkClassRec;

extern AsciiSinkClassRec asciiSinkClassRec;

/* New fields for the AsciiSink object record */
typedef struct {
    /* public resources */
    Boolean echo;
    Boolean display_nonprinting;

    /* private state */
    GC normgc, invgc, xorgc;
    Pixmap insertCursorOn;
    XawTextInsertState laststate;
    short cursor_x, cursor_y;	/* Cursor Location. */
} AsciiSinkPart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _AsciiSinkRec {
    ObjectPart          object;
    TextSinkPart	text_sink;
    AsciiSinkPart	ascii_sink;
} AsciiSinkRec;

#endif /* _XawAsciiSinkP_h */

