/*
 * $XConsortium: SmeP.h,v 1.4 89/12/11 15:20:22 kit Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * SmeP.h - Private Header file for Sme object.
 *
 * This is the private header file for the Athena Sme object.
 * This object is intended to be used with the simple menu widget.  
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 */

#ifndef _XawSmeP_h
#define _XawSmeP_h

/***********************************************************************
 *
 * Sme Widget Private Data
 *
 ***********************************************************************/

#include <X11/RectObjP.h>
#include <X11/Xaw/Sme.h>

/************************************************************
 *
 * New fields for the Sme widget class record.
 *
 ************************************************************/

typedef struct _SmeClassPart {
  void (*highlight)();
  void (*unhighlight)();
  void (*notify)();	
  XtPointer extension;
} SmeClassPart;

/* Full class record declaration */
typedef struct _SmeClassRec {
    RectObjClassPart    rect_class;
    SmeClassPart	sme_class;
} SmeClassRec;

extern SmeClassRec smeClassRec;

/* New fields for the Sme widget record */
typedef struct {
    /* resources */
    XtCallbackList callbacks;	/* The callback list */

} SmePart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _SmeRec {
  ObjectPart     object;
  RectObjPart    rectangle;
  SmePart	 sme;
} SmeRec;

/************************************************************
 *
 * Private declarations.
 *
 ************************************************************/

typedef void (*_XawEntryVoidFunc)();

#define XtInheritHighlight   ((_XawEntryVoidFunc) _XtInherit)
#define XtInheritUnhighlight XtInheritHighlight
#define XtInheritNotify      XtInheritHighlight

#endif /* _XawSmeP_h */
