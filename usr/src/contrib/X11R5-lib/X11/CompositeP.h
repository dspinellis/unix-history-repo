/* $XConsortium: CompositeP.h,v 1.15 91/10/24 13:20:18 converse Exp $ */

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

#ifndef _XtCompositeP_h
#define _XtCompositeP_h

#include <X11/Composite.h>

/************************************************************************
 *
 * Additional instance fields for widgets of (sub)class 'Composite' 
 *
 ************************************************************************/

typedef struct _CompositePart {
    WidgetList  children;	     /* array of ALL widget children	     */
    Cardinal    num_children;	     /* total number of widget children	     */
    Cardinal    num_slots;           /* number of slots in children array    */
    XtOrderProc insert_position;     /* compute position of new child	     */
} CompositePart,*CompositePtr;

typedef struct _CompositeRec {
    CorePart      core;
    CompositePart composite;
} CompositeRec;

/*********************************************************************
 *
 *  Additional class fields for widgets of (sub)class 'Composite'
 *
 ********************************************************************/

typedef struct _CompositeClassPart {
    XtGeometryHandler geometry_manager;	  /* geometry manager for children   */
    XtWidgetProc      change_managed;	  /* change managed state of child   */
    XtWidgetProc      insert_child;	  /* physically add child to parent  */
    XtWidgetProc      delete_child;	  /* physically remove child	     */
    XtPointer	      extension;	  /* pointer to extension record     */
} CompositeClassPart,*CompositePartPtr;

typedef struct {
    XtPointer next_extension;	/* 1st 4 mandated for all extension records */
    XrmQuark record_type;	/* NULLQUARK; on CompositeClassPart */
    long version;		/* must be XtCompositeExtensionVersion */
    Cardinal record_size;	/* sizeof(CompositeClassExtensionRec) */
    Boolean accepts_objects;
} CompositeClassExtensionRec, *CompositeClassExtension;


typedef struct _CompositeClassRec {
     CoreClassPart      core_class;
     CompositeClassPart composite_class;
} CompositeClassRec;

externalref CompositeClassRec compositeClassRec;

#define XtCompositeExtensionVersion 1L
#define XtInheritGeometryManager ((XtGeometryHandler) _XtInherit)
#define XtInheritChangeManaged ((XtWidgetProc) _XtInherit)
#define XtInheritInsertChild ((XtWidgetProc) _XtInherit)
#define XtInheritDeleteChild ((XtWidgetProc) _XtInherit)

#endif /* _XtCompositeP_h */
/* DON'T ADD STUFF AFTER THIS #endif */
