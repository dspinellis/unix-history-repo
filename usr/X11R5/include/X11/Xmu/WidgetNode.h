/*
 * $XConsortium: WidgetNode.h,v 1.7 91/07/22 23:46:16 converse Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
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
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

#ifndef _XmuWidgetNode_h
#define _XmuWidgetNode_h

#include <X11/Xfuncproto.h>

/*
 * This is usually initialized by setting the first two fields and letting
 * rest be implicitly nulled (by genlist.sh, for example)
 */
typedef struct _XmuWidgetNode {
    char *label;			/* mixed case name */
    WidgetClass *widget_class_ptr;	/* addr of widget class */
    struct _XmuWidgetNode *superclass;	/* superclass of widget_class */
    struct _XmuWidgetNode *children, *siblings;	/* subclass links */
    char *lowered_label;		/* lowercase version of label */
    char *lowered_classname;		/* lowercase version of class_name */
    Bool have_resources;		/* resources have been fetched */
    XtResourceList resources;		/* extracted resource database */
    struct _XmuWidgetNode **resourcewn;	/* where resources come from */
    Cardinal nresources;		/* number of resources */
    XtResourceList constraints;		/* extracted constraint resources */
    struct _XmuWidgetNode **constraintwn;  /* where constraints come from */
    Cardinal nconstraints;		/* number of constraint resources */
    XtPointer data;			/* extra data */
} XmuWidgetNode;

#define XmuWnClass(wn) ((wn)->widget_class_ptr[0])
#define XmuWnClassname(wn) (XmuWnClass(wn)->core_class.class_name)
#define XmuWnSuperclass(wn) ((XmuWnClass(wn))->core_class.superclass)

					/* external interfaces */
_XFUNCPROTOBEGIN

extern void XmuWnInitializeNodes (
#if NeedFunctionPrototypes
    XmuWidgetNode *	/* nodearray */,
    int			/* nnodes */
#endif
);

extern void XmuWnFetchResources (
#if NeedFunctionPrototypes
    XmuWidgetNode *	/* node */,
    Widget		/* toplevel */,
    XmuWidgetNode *	/* topnode */
#endif
);

extern int XmuWnCountOwnedResources (
#if NeedFunctionPrototypes
    XmuWidgetNode *	/* node */,
    XmuWidgetNode *	/* ownernode */,
    Bool		/* constraints */
#endif
);

extern XmuWidgetNode *XmuWnNameToNode (
#if NeedFunctionPrototypes
    XmuWidgetNode *	/* nodelist */,
    int			/* nnodes */,
    _Xconst char *	/* name */
#endif
);

_XFUNCPROTOEND

#endif /* _XmuWidgetNode_h */

