/* $XConsortium: IntrinsicP.h,v 1.57 91/06/26 19:33:20 converse Exp $ */

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

#ifndef _XtintrinsicP_h
#define _XtintrinsicP_h

#include <X11/Intrinsic.h>

typedef struct {
    XrmQuark	xrm_name;	  /* Resource name quark		*/
    XrmQuark	xrm_class;	  /* Resource class quark		*/
    XrmQuark	xrm_type;	  /* Resource representation type quark */
    Cardinal	xrm_size;	  /* Size in bytes of representation	*/
    long int	xrm_offset;	  /* -offset-1				*/
    XrmQuark	xrm_default_type; /* Default representation type quark	*/
    XtPointer	xrm_default_addr; /* Default resource address		*/
} XrmResource, *XrmResourceList;

typedef unsigned long XtVersionType;

#define XT_VERSION 11
#ifndef XT_REVISION
#define XT_REVISION 5
#endif
#define XtVersion (XT_VERSION * 1000 + XT_REVISION)
#define XtVersionDontCheck 0

typedef void (*XtProc)(
#if NeedFunctionPrototypes
    void
#endif
);

typedef void (*XtWidgetClassProc)(
#if NeedFunctionPrototypes
    WidgetClass /* class */
#endif
);

typedef void (*XtWidgetProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */
#endif
);

typedef Boolean (*XtAcceptFocusProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    Time*	/* time */
#endif
);

typedef void (*XtArgsProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    ArgList	/* args */,
    Cardinal*	/* num_args */
#endif
);

typedef void (*XtInitProc)(
#if NeedFunctionPrototypes
    Widget	/* request */,
    Widget	/* new */,
    ArgList	/* args */,
    Cardinal*	/* num_args */
#endif
);

typedef Boolean (*XtSetValuesFunc)(
#if NeedFunctionPrototypes
    Widget 	/* old */,
    Widget 	/* request */,
    Widget 	/* new */,
    ArgList 	/* args */,
    Cardinal*	/* num_args */
#endif
);

typedef Boolean (*XtArgsFunc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    ArgList	/* args */,
    Cardinal*	/* num_args */
#endif
);

typedef void (*XtAlmostProc)(
#if NeedFunctionPrototypes
    Widget		/* old */,
    Widget		/* new */,
    XtWidgetGeometry*	/* request */,
    XtWidgetGeometry*	/* reply */
#endif
);

typedef void (*XtExposeProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    XEvent*	/* event */,
    Region	/* region */
#endif
);

/* compress_exposure options*/
#define XtExposeNoCompress		((XtEnum)False)
#define XtExposeCompressSeries		((XtEnum)True)
#define XtExposeCompressMultiple	2
#define XtExposeCompressMaximal		3

/* modifiers */
#define XtExposeGraphicsExpose	  	0x10
#define XtExposeGraphicsExposeMerged	0x20
#define XtExposeNoExpose	  	0x40


typedef void (*XtRealizeProc)(
#if NeedFunctionPrototypes
    Widget 		  /* widget */,
    XtValueMask* 	  /* mask */,
    XSetWindowAttributes* /* attributes */
#endif
);

typedef XtGeometryResult (*XtGeometryHandler)(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XtWidgetGeometry*	/* request */,
    XtWidgetGeometry*	/* reply */
#endif
);

typedef void (*XtStringProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    String	/* str */
#endif
);

typedef struct _XtTMRec {
    XtTranslations  translations;	/* private to Translation Manager    */
    XtBoundActions  proc_table;		/* procedure bindings for actions    */
    struct _XtStateRec *current_state;  /* Translation Manager state ptr     */
    unsigned long   lastEventTime;
} XtTMRec, *XtTM;

#include <X11/CoreP.h>
#include <X11/CompositeP.h>
#include <X11/ConstrainP.h>
#include <X11/ObjectP.h>
#include <X11/RectObjP.h>

#define XtDisplay(widget)	DisplayOfScreen((widget)->core.screen)
#define XtScreen(widget)	((widget)->core.screen)
#define XtWindow(widget)	((widget)->core.window)

#define XtClass(widget)		((widget)->core.widget_class)
#define XtSuperclass(widget)	(XtClass(widget)->core_class.superclass)
#define XtIsRealized(object)	(XtWindowOfObject(object) != None)
#define XtParent(widget)	((widget)->core.parent)

#undef XtIsRectObj
#define XtIsRectObj(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x02)

#undef XtIsWidget
#define XtIsWidget(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x04)

#undef XtIsComposite
#define XtIsComposite(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x08)

#undef XtIsConstraint
#define XtIsConstraint(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x10)

#undef XtIsShell
#define XtIsShell(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x20)

#undef XtIsWMShell
#define XtIsWMShell(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x40)

#undef XtIsTopLevelShell
#define XtIsTopLevelShell(obj) \
    (((Object)(obj))->object.widget_class->core_class.class_inited & 0x80)

#ifdef DEBUG
#define XtCheckSubclass(w, widget_class_ptr, message)	\
	if (!XtIsSubclass(((Widget)(w)), (widget_class_ptr))) {	\
	    String params[3];				\
	    Cardinal num_params = 3;			\
	    params[0] = ((Widget)(w))->core.widget_class->core_class.class_name;\
	    params[1] = (widget_class_ptr)->core_class.class_name;	     \
	    params[2] = (message);					     \
	    XtAppErrorMsg(XtWidgetToApplicationContext((Widget)(w)),	     \
		    "subclassMismatch", "xtCheckSubclass", "XtToolkitError", \
		    "Widget class %s found when subclass of %s expected: %s",\
		    params, &num_params);		\
	}
#else
#define XtCheckSubclass(w, widget_class, message)	/* nothing */
#endif

_XFUNCPROTOBEGIN

extern Widget _XtWindowedAncestor( /* internal; implementation-dependent */
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern void _XtInherit(
#if NeedFunctionPrototypes
    void
#endif
);

extern void XtCreateWindow(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    unsigned int 	/* window_class */,
    Visual*		/* visual */,
    XtValueMask		/* value_mask */,
    XSetWindowAttributes* /* attributes */
#endif
);

extern void XtResizeWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtDimension	/* width */,
    _XtDimension	/* height */,
    _XtDimension	/* border_width */
#endif
);

extern void XtMoveWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtPosition		/* x */,
    _XtPosition		/* y */
#endif
);

extern void XtConfigureWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtPosition		/* x */,
    _XtPosition		/* y */,
    _XtDimension	/* width */,
    _XtDimension	/* height */,
    _XtDimension	/* border_width */
#endif
);

extern void XtResizeWindow(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

_XFUNCPROTOEND

#endif /* _XtIntrinsicP_h */
/* DON'T ADD STUFF AFTER THIS #endif */
