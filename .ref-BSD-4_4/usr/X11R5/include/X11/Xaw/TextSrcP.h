/*
* $XConsortium: TextSrcP.h,v 1.18 91/02/20 17:58:06 converse Exp $
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
 * TextSrcP.h - Private definitions for TextSrc object
 * 
 */

#ifndef _XawTextSrcP_h
#define _XawTextSrcP_h

/***********************************************************************
 *
 * TextSrc Object Private Data
 *
 ***********************************************************************/

#include <X11/ObjectP.h>
#include <X11/Xaw/TextP.h>	/* This source works with the Text widget. */
#include <X11/Xaw/TextSrc.h>

/************************************************************
 *
 * New fields for the TextSrc object class record.
 *
 ************************************************************/

typedef struct _TextSrcClassPart {
  XawTextPosition	(*Read)();
  int			(*Replace)();
  XawTextPosition	(*Scan)();
  XawTextPosition       (*Search)();
  void                  (*SetSelection)();
  Boolean		(*ConvertSelection)();
} TextSrcClassPart;

/* Full class record declaration */
typedef struct _TextSrcClassRec {
    ObjectClassPart     object_class;
    TextSrcClassPart	textSrc_class;
} TextSrcClassRec;

extern TextSrcClassRec textSrcClassRec;

/* New fields for the TextSrc object record */
typedef struct {
    /* resources */
  XawTextEditType	edit_mode;
} TextSrcPart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _TextSrcRec {
  ObjectPart    object;
  TextSrcPart	textSrc;
} TextSrcRec;

/************************************************************
 *
 * Private declarations.
 *
 ************************************************************/

typedef Boolean (*_XawBooleanFunc)();
typedef int (*_XawIntFunc)();
typedef XawTextPosition (*_XawTextPositionFunc)();
typedef void (*_XawTextVoidFunc)();

#define XtInheritRead                 ((_XawTextPositionFunc) _XtInherit)
#define XtInheritReplace              ((_XawIntFunc) _XtInherit)
#define XtInheritScan                 ((_XawTextPositionFunc) _XtInherit)
#define XtInheritSearch               ((_XawTextPositionFunc) _XtInherit)
#define XtInheritSetSelection         ((_XawTextVoidFunc) _XtInherit)
#define XtInheritConvertSelection     ((_XawBooleanFunc) _XtInherit)

#endif /* _XawTextSrcP_h */
