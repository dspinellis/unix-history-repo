/* $XConsortium: Intrinsic.h,v 1.174 91/09/09 16:25:56 converse Exp $ */

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

#ifndef _XtIntrinsic_h
#define _XtIntrinsic_h

#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<X11/Xresource.h>
#include	<X11/Xfuncproto.h>
#ifdef XT_BC
#include <X11/Xos.h>		/* for R4 compatibility */
#else
#include <X11/Xosdefs.h>
#ifndef X_NOT_STDC_ENV
#include <string.h>		/* for XtNewString */
#else
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif /* SYSV else */
#endif /* !X_NOT_STDC_ENV else */
#endif /* XT_BC else */

#define XtSpecificationRelease 5

typedef char *String;

#if NeedFunctionPrototypes

/* We do this in order to get "const" declarations to work right.  We
 * use _XtString instead of String so that C++ applications can
 * #define String to something else if they choose, to avoid conflicts
 * with other C++ libraries.
 */
#define _XtString char*

/* _Xt names are private to Xt implementation, do not use in client code */
#if NeedWidePrototypes
#define _XtBoolean	int
#define _XtDimension	unsigned int
#define _XtKeyCode	unsigned int
#define _XtPosition	int
#define _XtXtEnum	unsigned int
#else
#define _XtBoolean	Boolean
#define _XtDimension	Dimension
#define _XtKeyCode	KeyCode
#define _XtPosition	Position
#define _XtXtEnum	XtEnum
#endif /* NeedWidePrototypes */

#endif /* NeedFunctionPrototypes */

#ifndef NULL
#define NULL 0
#endif

#ifdef VMS
#define externalref globalref
#define externaldef(psect) globaldef {"psect"} noshare
#else
#define externalref extern
#define externaldef(psect)
#endif /* VMS */

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#define XtNumber(arr)		((Cardinal) (sizeof(arr) / sizeof(arr[0])))

typedef struct _WidgetRec *Widget;
typedef Widget *WidgetList;
typedef struct _WidgetClassRec *WidgetClass;
typedef struct _CompositeRec *CompositeWidget;
typedef struct _XtActionsRec *XtActionList;
typedef struct _XtEventRec *XtEventTable;
typedef struct _XtBoundAccActionRec *XtBoundAccActions;

typedef struct _XtAppStruct *XtAppContext;
typedef unsigned long	XtValueMask;
typedef unsigned long	XtIntervalId;
typedef unsigned long	XtInputId;
typedef unsigned long	XtWorkProcId;
typedef unsigned int	XtGeometryMask;
typedef unsigned long	XtGCMask;   /* Mask of values that are used by widget*/
typedef unsigned long	Pixel;	    /* Index into colormap		*/
typedef int		XtCacheType;
#define			XtCacheNone	  0x001
#define			XtCacheAll	  0x002
#define			XtCacheByDisplay  0x003
#define			XtCacheRefCount	  0x100

/****************************************************************
 *
 * System Dependent Definitions; see spec for specific range
 * requirements.  Do not assume every implementation uses the
 * same base types!
 *
 *
 * XtArgVal ought to be a union of XtPointer, char *, long, int *, and proc *
 * but casting to union types is not really supported.
 *
 * So the typedef for XtArgVal should be chosen such that
 *
 *	sizeof (XtArgVal) >=	sizeof(XtPointer)
 *				sizeof(char *)
 *				sizeof(long)
 *				sizeof(int *)
 *				sizeof(proc *)
 *
 * ArgLists rely heavily on the above typedef.
 *
 ****************************************************************/
#ifdef CRAY
typedef long		Boolean;
typedef char*		XtArgVal;
typedef long		XtEnum;
#else
typedef char		Boolean;
typedef long		XtArgVal;
typedef unsigned char	XtEnum;
#endif

typedef unsigned int	Cardinal;
typedef unsigned short	Dimension;  /* Size in pixels			*/
typedef short		Position;   /* Offset from 0 coordinate		*/

#if NeedFunctionPrototypes
typedef void*		XtPointer;
#else
typedef char*		XtPointer;
#endif

/* The type Opaque is NOT part of the Xt standard, do NOT use it. */
/* (It remains here only for backward compatibility.) */
typedef XtPointer	Opaque;

#include <X11/Core.h>
#include <X11/Composite.h>
#include <X11/Constraint.h>
#include <X11/Object.h>
#include <X11/RectObj.h>

typedef struct _TranslationData *XtTranslations;
typedef struct _TranslationData *XtAccelerators;
typedef unsigned int Modifiers;

typedef void (*XtActionProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

typedef XtActionProc* XtBoundActions;

typedef struct _XtActionsRec{
    String	 string;
    XtActionProc proc;
} XtActionsRec;

typedef enum {
/* address mode		parameter representation    */
/* ------------		------------------------    */
    XtAddress,		/* address		    */
    XtBaseOffset,	/* offset		    */
    XtImmediate,	/* constant		    */
    XtResourceString,	/* resource name string	    */
    XtResourceQuark,	/* resource name quark	    */
    XtWidgetBaseOffset,	/* offset from ancestor	    */
    XtProcedureArg	/* procedure to invoke	    */
} XtAddressMode;

typedef struct {
    XtAddressMode   address_mode;
    XtPointer	    address_id;
    Cardinal	    size;
} XtConvertArgRec, *XtConvertArgList;

typedef void (*XtConvertArgProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Cardinal*		/* size */,
    XrmValue*		/* value */
#endif
);

typedef struct {
    XtGeometryMask request_mode;
    Position x, y;
    Dimension width, height, border_width;
    Widget sibling;
    int stack_mode;   /* Above, Below, TopIf, BottomIf, Opposite, DontChange */
} XtWidgetGeometry;

/* Additions to Xlib geometry requests: ask what would happen, don't do it */
#define XtCWQueryOnly	(1 << 7)

/* Additions to Xlib stack modes: don't change stack order */
#define XtSMDontChange	5

typedef void (*XtConverter)( /* obsolete */
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValue*		/* from */,
    XrmValue*		/* to */
#endif
);

typedef Boolean (*XtTypeConverter)(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValue*		/* from */,
    XrmValue*		/* to */,
    XtPointer*		/* converter_data */
#endif
);

typedef void (*XtDestructor)(
#if NeedFunctionPrototypes
    XtAppContext	/* app */,
    XrmValue*		/* to */,
    XtPointer 		/* converter_data */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */
#endif
);

typedef Opaque XtCacheRef;

typedef Opaque XtActionHookId;

typedef void (*XtActionHookProc)(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XtPointer		/* client_data */,
    String		/* action_name */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

typedef void (*XtKeyProc)(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    _XtKeyCode 		/* keycode */,
    Modifiers		/* modifiers */,
    Modifiers*		/* modifiers_return */,
    KeySym*		/* keysym_return */
#endif
);

typedef void (*XtCaseProc)(
#if NeedFunctionPrototypes
    Display*		/* display */,
    KeySym		/* keysym */,
    KeySym*		/* lower_return */,
    KeySym*		/* upper_return */
#endif
);

typedef void (*XtEventHandler)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    XEvent*		/* event */,
    Boolean*		/* continue_to_dispatch */
#endif
);
typedef unsigned long EventMask;

typedef enum {XtListHead, XtListTail } XtListPosition;

typedef unsigned long	XtInputMask;
#define XtInputNoneMask		0L
#define XtInputReadMask		(1L<<0)
#define XtInputWriteMask	(1L<<1)
#define XtInputExceptMask	(1L<<2)

typedef void (*XtTimerCallbackProc)(
#if NeedFunctionPrototypes
    XtPointer 		/* closure */,
    XtIntervalId*	/* id */
#endif
);

typedef void (*XtInputCallbackProc)(
#if NeedFunctionPrototypes
    XtPointer 		/* closure */,
    int*		/* source */,
    XtInputId*		/* id */
#endif
);

typedef struct {
    String	name;
    XtArgVal	value;
} Arg, *ArgList;

typedef XtPointer	XtVarArgsList;

typedef void (*XtCallbackProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,	/* data the application registered */
    XtPointer 		/* call_data */	/* callback specific data */
#endif
);

typedef struct _XtCallbackRec {
    XtCallbackProc  callback;
    XtPointer	    closure;
} XtCallbackRec, *XtCallbackList;

typedef enum {
	XtCallbackNoList,
	XtCallbackHasNone,
	XtCallbackHasSome
} XtCallbackStatus;

typedef enum  {
    XtGeometryYes,	  /* Request accepted. */
    XtGeometryNo,	  /* Request denied. */
    XtGeometryAlmost,	  /* Request denied, but willing to take replyBox. */
    XtGeometryDone	  /* Request accepted and done. */
} XtGeometryResult;

typedef enum {XtGrabNone, XtGrabNonexclusive, XtGrabExclusive} XtGrabKind;

typedef struct {
    Widget  shell_widget;
    Widget  enable_widget;
} XtPopdownIDRec, *XtPopdownID;

typedef struct _XtResource {
    String	resource_name;	/* Resource name			    */
    String	resource_class;	/* Resource class			    */
    String	resource_type;	/* Representation type desired		    */
    Cardinal	resource_size;	/* Size in bytes of representation	    */
    Cardinal	resource_offset;/* Offset from base to put resource value   */
    String	default_type;	/* representation type of specified default */
    XtPointer	default_addr;	/* Address of default resource		    */
} XtResource, *XtResourceList;

typedef void (*XtResourceDefaultProc)(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    int		/* offset */,
    XrmValue*	/* value */
#endif
);

typedef String (*XtLanguageProc)(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    String	/* xnl */,
    XtPointer	/* client_data */
#endif
);

typedef void (*XtErrorMsgHandler)(
#if NeedFunctionPrototypes
    String 		/* name */,
    String		/* type */,
    String		/* class */,
    String		/* default */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

typedef void (*XtErrorHandler)(
#if NeedFunctionPrototypes
  String		/* msg */
#endif
);

typedef void (*XtCreatePopupChildProc)(
#if NeedFunctionPrototypes
    Widget	/* shell */
#endif
);

typedef Boolean (*XtWorkProc)(
#if NeedFunctionPrototypes
    XtPointer 		/* closure */	/* data the application registered */
#endif
);

typedef struct {
    char match;
    String substitution;
} SubstitutionRec, *Substitution;

typedef Boolean (*XtFilePredicate)(
#if NeedFunctionPrototypes
   String /* filename */
#endif
);

typedef XtPointer XtRequestId;

typedef Boolean (*XtConvertSelectionProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    Atom*		/* target */,
    Atom*		/* type_return */,
    XtPointer*		/* value_return */,
    unsigned long*	/* length_return */,
    int*		/* format_return */
#endif
);

typedef void (*XtLoseSelectionProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */
#endif
);

typedef void (*XtSelectionDoneProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    Atom*		/* target */
#endif
);

typedef void (*XtSelectionCallbackProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    Atom*		/* selection */,
    Atom*		/* type */,
    XtPointer 		/* value */,
    unsigned long*	/* length */,
    int*		/* format */
#endif
);

typedef void (*XtLoseSelectionIncrProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    XtPointer 		/* client_data */
#endif
);

typedef void (*XtSelectionDoneIncrProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    Atom*		/* target */,
    XtRequestId*	/* receiver_id */,
    XtPointer 		/* client_data */
#endif
);

typedef Boolean (*XtConvertSelectionIncrProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    Atom*		/* target */,
    Atom*		/* type */,
    XtPointer*		/* value */,
    unsigned long*	/* length */,
    int*		/* format */,
    unsigned long*	/* max_length */,
    XtPointer 		/* client_data */,
    XtRequestId*	/* receiver_id */
#endif
);

typedef void (*XtCancelConvertSelectionProc)(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom*		/* selection */,
    Atom*		/* target */,
    XtRequestId*	/* receiver_id */,
    XtPointer 		/* client_data */
#endif
);

/***************************************************************
 *
 * Exported Interfaces
 *
 ****************************************************************/

_XFUNCPROTOBEGIN

extern Boolean XtConvertAndStore(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* from_type */,
    XrmValue*		/* from */,
    _Xconst _XtString 	/* to_type */,
    XrmValue*		/* to_in_out */
#endif
);

extern Boolean XtCallConverter(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XtTypeConverter 	/* converter */,
    XrmValuePtr 	/* args */,
    Cardinal 		/* num_args */,
    XrmValuePtr 	/* from */,
    XrmValue*		/* to_in_out */,
    XtCacheRef*		/* cache_ref_return */
#endif
);

extern Boolean XtDispatchEvent(
#if NeedFunctionPrototypes
    XEvent* 		/* event */
#endif
);

extern Boolean XtCallAcceptFocus(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Time*		/* time */
#endif
);

extern Boolean XtPeekEvent( /* obsolete */
#if NeedFunctionPrototypes
    XEvent*		/* event_return */
#endif
);

extern Boolean XtAppPeekEvent(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XEvent*		/* event_return */
#endif
);

extern Boolean XtIsSubclass(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    WidgetClass 	/* widgetClass */
#endif
);

extern Boolean XtIsObject(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern Boolean _XtCheckSubclassFlag( /* implementation-private */
#if NeedFunctionPrototypes
    Widget		/* object */,
    _XtXtEnum		/* type_flag */
#endif
);

extern Boolean _XtIsSubclassOf( /* implementation-private */
#if NeedFunctionPrototypes
    Widget		/* object */,
    WidgetClass		/* widget_class */,
    WidgetClass		/* flag_class */,
    _XtXtEnum		/* type_flag */
#endif
);

extern Boolean XtIsManaged(
#if NeedFunctionPrototypes
    Widget 		/* rectobj */
#endif
);

extern Boolean XtIsRealized(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern Boolean XtIsSensitive(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern Boolean XtOwnSelection(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Time 		/* time */,
    XtConvertSelectionProc /* convert */,
    XtLoseSelectionProc	/* lose */,
    XtSelectionDoneProc /* done */
#endif
);

extern Boolean XtOwnSelectionIncremental(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Time 		/* time */,
    XtConvertSelectionIncrProc	/* convert_callback */,
    XtLoseSelectionIncrProc	/* lose_callback */,
    XtSelectionDoneIncrProc	/* done_callback */,
    XtCancelConvertSelectionProc /* cancel_callback */,
    XtPointer 		/* client_data */
#endif
);

extern XtGeometryResult XtMakeResizeRequest(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtDimension	/* width */,
    _XtDimension	/* height */,
    Dimension*		/* width_return */,
    Dimension*		/* height_return */
#endif
);

extern void XtTranslateCoords(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtPosition		/* x */,
    _XtPosition		/* y */,
    Position*		/* rootx_return */,
    Position*		/* rooty_return */
#endif
);

extern KeySym* XtGetKeysymTable(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    KeyCode*		/* min_keycode_return */,
    int*		/* keysyms_per_keycode_return */
#endif
);

extern void XtKeysymToKeycodeList(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    KeySym 		/* keysym */,
    KeyCode**		/* keycodes_return */,
    Cardinal*		/* keycount_return */
#endif
);

extern void XtStringConversionWarning( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* from_value */,
    _Xconst _XtString	/* to_type */
#endif
);

extern void XtDisplayStringConversionWarning(
#if NeedFunctionPrototypes
    Display*	 	/* dpy */,
    _Xconst _XtString	/* from_value */,
    _Xconst _XtString	/* to_type */
#endif
);

#if __STDC__
externalref XtConvertArgRec const colorConvertArgs[];
externalref XtConvertArgRec const screenConvertArg[];
#else
externalref XtConvertArgRec colorConvertArgs[];
externalref XtConvertArgRec screenConvertArg[];
#endif

extern void XtAppAddConverter( /* obsolete */
#if NeedFunctionPrototypes
    XtAppContext	/* app_context */,
    _Xconst _XtString	/* from_type */,
    _Xconst _XtString	/* to_type */,
    XtConverter 	/* converter */,
    XtConvertArgList	/* convert_args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtAddConverter( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* from_type */,
    _Xconst _XtString 	/* to_type */,
    XtConverter 	/* converter */,
    XtConvertArgList 	/* convert_args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtSetTypeConverter(
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* from_type */,
    _Xconst _XtString 	/* to_type */,
    XtTypeConverter 	/* converter */,
    XtConvertArgList 	/* convert_args */,
    Cardinal 		/* num_args */,
    XtCacheType 	/* cache_type */,
    XtDestructor 	/* destructor */
#endif
);

extern void XtAppSetTypeConverter(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString 	/* from_type */,
    _Xconst _XtString 	/* to_type */,
    XtTypeConverter 	/* converter */,
    XtConvertArgList 	/* convert_args */,
    Cardinal 		/* num_args */,
    XtCacheType 	/* cache_type */,
    XtDestructor 	/* destructor */
#endif
);

extern void XtConvert( /* obsolete */
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* from_type */,
    XrmValue*		/* from */,
    _Xconst _XtString 	/* to_type */,
    XrmValue*		/* to_return */
#endif
);

extern void XtDirectConvert( /* obsolete */
#if NeedFunctionPrototypes
    XtConverter 	/* converter */,
    XrmValuePtr 	/* args */,
    Cardinal 		/* num_args */,
    XrmValuePtr 	/* from */,
    XrmValue*		/* to_return */
#endif
);

/****************************************************************
 *
 * Translation Management
 *
 ****************************************************************/

extern XtTranslations XtParseTranslationTable(
#if NeedFunctionPrototypes
    _Xconst _XtString	/* table */
#endif
);

extern XtAccelerators XtParseAcceleratorTable(
#if NeedFunctionPrototypes
    _Xconst _XtString	/* source */
#endif
);

extern void XtOverrideTranslations(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtTranslations 	/* translations */
#endif
);

extern void XtAugmentTranslations(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtTranslations 	/* translations */
#endif
);

extern void XtInstallAccelerators(
#if NeedFunctionPrototypes
    Widget 		/* destination */,
    Widget		/* source */
#endif
);

extern void XtInstallAllAccelerators(
#if NeedFunctionPrototypes
    Widget 		/* destination */,
    Widget		/* source */
#endif
);

extern void XtUninstallTranslations(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern void XtAppAddActions(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtActionList 	/* actions */,
    Cardinal 		/* num_actions */
#endif
);

extern void XtAddActions( /* obsolete */
#if NeedFunctionPrototypes
    XtActionList 	/* actions */,
    Cardinal 		/* num_actions */
#endif
);

extern XtActionHookId XtAppAddActionHook(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtActionHookProc 	/* proc */,
    XtPointer 		/* client_data */
#endif
);

extern void XtRemoveActionHook(
#if NeedFunctionPrototypes
    XtActionHookId 	/* id */
#endif
);

extern void XtGetActionList(
#if NeedFunctionPrototypes
    WidgetClass		/* widget_class */,
    XtActionList*	/* actions_return */,
    Cardinal*		/* num_actions_return */
#endif
);

extern void XtCallActionProc(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    _Xconst _XtString	/* action */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal		/* num_params */
#endif
);

extern void XtRegisterGrabAction(
#if NeedFunctionPrototypes
    XtActionProc 	/* action_proc */,
    _XtBoolean 		/* owner_events */,
    unsigned int 	/* event_mask */,
    int			/* pointer_mode */,
    int	 		/* keyboard_mode */
#endif
);

extern void XtSetMultiClickTime(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int 		/* milliseconds */
#endif
);

extern int XtGetMultiClickTime(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

extern KeySym XtGetActionKeysym(
#if NeedFunctionPrototypes
    XEvent*		/* event */,
    Modifiers*		/* modifiers_return */
#endif
);

/***************************************************************
 *
 * Keycode and Keysym procedures for translation management
 *
 ****************************************************************/

extern void XtTranslateKeycode(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    _XtKeyCode 		/* keycode */,
    Modifiers 		/* modifiers */,
    Modifiers*		/* modifiers_return */,
    KeySym*		/* keysym_return */
#endif
);

extern void XtTranslateKey(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    _XtKeyCode		/* keycode */,
    Modifiers		/* modifiers */,
    Modifiers*		/* modifiers_return */,
    KeySym*		/* keysym_return */
#endif
);

extern void XtSetKeyTranslator(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XtKeyProc 		/* proc */
#endif
);

extern void XtRegisterCaseConverter(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XtCaseProc 		/* proc */,
    KeySym 		/* start */,
    KeySym 		/* stop */
#endif
);

extern void XtConvertCase(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    KeySym 		/* keysym */,
    KeySym*		/* lower_return */,
    KeySym*		/* upper_return */
#endif
);

/****************************************************************
 *
 * Event Management
 *
 ****************************************************************/

/* XtAllEvents is valid only for XtRemoveEventHandler and
 * XtRemoveRawEventHandler; don't use it to select events!
 */
#define XtAllEvents ((EventMask) -1L)

extern void XtAddEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtRemoveEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtAddRawEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtRemoveRawEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtInsertEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */,
    XtListPosition 	/* position */
#endif
);

extern void XtInsertRawEventHandler(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    EventMask 		/* event_mask */,
    _XtBoolean 		/* nonmaskable */,
    XtEventHandler 	/* proc */,
    XtPointer 		/* closure */,
    XtListPosition 	/* position */
#endif
);

extern EventMask XtBuildEventMask(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern void XtAddGrab(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtBoolean 		/* exclusive */,
    _XtBoolean 		/* spring_loaded */
#endif
);

extern void XtRemoveGrab(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern void XtProcessEvent( /* obsolete */
#if NeedFunctionPrototypes
    XtInputMask 		/* mask */
#endif
);

extern void XtAppProcessEvent(
#if NeedFunctionPrototypes
    XtAppContext 		/* app_context */,
    XtInputMask 		/* mask */
#endif
);

extern void XtMainLoop( /* obsolete */
#if NeedFunctionPrototypes
    void
#endif
);

extern void XtAppMainLoop(
#if NeedFunctionPrototypes
    XtAppContext 		/* app_context */
#endif
);

extern void XtAddExposureToRegion(
#if NeedFunctionPrototypes
    XEvent*		/* event */,
    Region 		/* region */
#endif
);

extern void XtSetKeyboardFocus(
#if NeedFunctionPrototypes
    Widget		/* subtree */,
    Widget 		/* descendent */
#endif
);

extern Time XtLastTimestampProcessed(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

/****************************************************************
 *
 * Event Gathering Routines
 *
 ****************************************************************/

extern XtIntervalId XtAddTimeOut( /* obsolete */
#if NeedFunctionPrototypes
    unsigned long 	/* interval */,
    XtTimerCallbackProc /* proc */,
    XtPointer 		/* closure */
#endif
);

extern XtIntervalId XtAppAddTimeOut(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    unsigned long 	/* interval */,
    XtTimerCallbackProc /* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtRemoveTimeOut(
#if NeedFunctionPrototypes
    XtIntervalId 	/* timer */
#endif
);

extern XtInputId XtAddInput( /* obsolete */
#if NeedFunctionPrototypes
    int 		/* source */,
    XtPointer 		/* condition */,
    XtInputCallbackProc /* proc */,
    XtPointer 		/* closure */
#endif
);

extern XtInputId XtAppAddInput(
#if NeedFunctionPrototypes
    XtAppContext       	/* app_context */,
    int 		/* source */,
    XtPointer 		/* condition */,
    XtInputCallbackProc /* proc */,
    XtPointer 		/* closure */
#endif
);

extern void XtRemoveInput(
#if NeedFunctionPrototypes
    XtInputId 		/* id */
#endif
);

extern void XtNextEvent( /* obsolete */
#if NeedFunctionPrototypes
    XEvent* 		/* event */
#endif
);

extern void XtAppNextEvent(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XEvent*		/* event_return */
#endif
);

#define XtIMXEvent		1
#define XtIMTimer		2
#define XtIMAlternateInput	4
#define XtIMAll (XtIMXEvent | XtIMTimer | XtIMAlternateInput)

extern XtInputMask XtPending( /* obsolete */
#if NeedFunctionPrototypes
    void
#endif
);

extern XtInputMask XtAppPending(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */
#endif
);

/****************************************************************
 *
 * Random utility routines
 *
 ****************************************************************/

#define XtIsRectObj(object)	(_XtCheckSubclassFlag(object, (XtEnum)0x02))
#define XtIsWidget(object)	(_XtCheckSubclassFlag(object, (XtEnum)0x04))
#define XtIsComposite(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x08))
#define XtIsConstraint(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x10))
#define XtIsShell(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x20))
#define XtIsOverrideShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)overrideShellWidgetClass, \
		     (WidgetClass)shellWidgetClass, (XtEnum)0x20))
#define XtIsWMShell(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x40))
#define XtIsVendorShell(widget)	\
    (_XtIsSubclassOf(widget, (WidgetClass)vendorShellWidgetClass, \
		     (WidgetClass)wmShellWidgetClass, (XtEnum)0x40))
#define XtIsTransientShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)transientShellWidgetClass, \
		     (WidgetClass)wmShellWidgetClass, (XtEnum)0x40))
#define XtIsTopLevelShell(widget) (_XtCheckSubclassFlag(widget, (XtEnum)0x80))
#define XtIsApplicationShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)applicationShellWidgetClass, \
		     (WidgetClass)topLevelShellWidgetClass, (XtEnum)0x80))

extern void XtRealizeWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

void XtUnrealizeWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern void XtDestroyWidget(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern void XtSetSensitive(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtBoolean 		/* sensitive */
#endif
);

extern void XtSetMappedWhenManaged(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtBoolean 		/* mapped_when_managed */
#endif
);

extern Widget XtNameToWidget(
#if NeedFunctionPrototypes
    Widget 		/* reference */,
    _Xconst _XtString	/* names */
#endif
);

extern Widget XtWindowToWidget(
#if NeedFunctionPrototypes
    Display*		/* display */,
    Window 		/* window */
#endif
);

/***************************************************************
 *
 * Arg lists
 *
 ****************************************************************/


#define XtSetArg(arg, n, d) \
    ((void)( (arg).name = (n), (arg).value = (XtArgVal)(d) ))

extern ArgList XtMergeArgLists(
#if NeedFunctionPrototypes
    ArgList 		/* args1 */,
    Cardinal 		/* num_args1 */,
    ArgList 		/* args2 */,
    Cardinal 		/* num_args2 */
#endif
);

/***************************************************************
 *
 * Vararg lists
 *
 ****************************************************************/

#define XtVaNestedList  "XtVaNestedList"
#define XtVaTypedArg    "XtVaTypedArg"

extern XtVarArgsList XtVaCreateArgsList(
#if NeedVarargsPrototypes
    XtPointer		/*unused*/, ...
#endif
);

/*************************************************************
 *
 * Information routines
 *
 ************************************************************/

#ifndef _XtIntrinsicP_h

/* We're not included from the private file, so define these */

extern Display *XtDisplay(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern Display *XtDisplayOfObject(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern Screen *XtScreen(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern Screen *XtScreenOfObject(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern Window XtWindow(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern Window XtWindowOfObject(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern String XtName(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern WidgetClass XtSuperclass(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern WidgetClass XtClass(
#if NeedFunctionPrototypes
    Widget 		/* object */
#endif
);

extern Widget XtParent(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

#endif /*_XtIntrinsicP_h*/

#define XtMapWidget(widget)	XMapWindow(XtDisplay(widget), XtWindow(widget))
#define XtUnmapWidget(widget)	\
		XUnmapWindow(XtDisplay(widget), XtWindow(widget))

extern void XtAddCallback(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */,
    XtCallbackProc 	/* callback */,
    XtPointer 		/* closure */
#endif
);

extern void XtRemoveCallback(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */,
    XtCallbackProc 	/* callback */,
    XtPointer 		/* closure */
#endif
);

extern void XtAddCallbacks(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString	/* callback_name */,
    XtCallbackList 	/* callbacks */
#endif
);

extern void XtRemoveCallbacks(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */,
    XtCallbackList 	/* callbacks */
#endif
);

extern void XtRemoveAllCallbacks(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */
#endif
);


extern void XtCallCallbacks(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */,
    XtPointer 		/* call_data */
#endif
);

extern void XtCallCallbackList(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XtCallbackList 	/* callbacks */,
    XtPointer 		/* call_data */
#endif
);

extern XtCallbackStatus XtHasCallbacks(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _Xconst _XtString 	/* callback_name */
#endif
);

/****************************************************************
 *
 * Geometry Management
 *
 ****************************************************************/


extern XtGeometryResult XtMakeGeometryRequest(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtWidgetGeometry*	/* request */,
    XtWidgetGeometry*	/* reply_return */
#endif
);

extern XtGeometryResult XtQueryGeometry(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtWidgetGeometry*	/* intended */,
    XtWidgetGeometry*	/* preferred_return */
#endif
);

extern Widget XtCreatePopupShell(
#if NeedFunctionPrototypes
    _Xconst _XtString	/* name */,
    WidgetClass 	/* widgetClass */,
    Widget 		/* parent */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtVaCreatePopupShell(
#if NeedVarargsPrototypes
    _Xconst _XtString	/* name */,
    WidgetClass		/* widgetClass */,
    Widget		/* parent */,
    ...
#endif
);

extern void XtPopup(
#if NeedFunctionPrototypes
    Widget 		/* popup_shell */,
    XtGrabKind 		/* grab_kind */
#endif
);

extern void XtPopupSpringLoaded(
#if NeedFunctionPrototypes
    Widget 		/* popup_shell */
#endif
);

extern void XtCallbackNone(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    XtPointer 		/* call_data */
#endif
);

extern void XtCallbackNonexclusive(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    XtPointer 		/* call_data */
#endif
);

extern void XtCallbackExclusive(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    XtPointer 		/* call_data */
#endif
);

extern void XtPopdown(
#if NeedFunctionPrototypes
    Widget 		/* popup_shell */
#endif
);

extern void XtCallbackPopdown(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,
    XtPointer 		/* call_data */
#endif
);

extern void XtMenuPopupAction(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern Widget XtCreateWidget(
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* name */,
    WidgetClass 	/* widget_class */,
    Widget 		/* parent */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtCreateManagedWidget(
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* name */,
    WidgetClass 	/* widget_class */,
    Widget 		/* parent */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtVaCreateWidget(
#if NeedVarargsPrototypes
    _Xconst _XtString	/* name */,
    WidgetClass		/* widget */,
    Widget		/* parent */,
    ...
#endif
);

extern Widget XtVaCreateManagedWidget(
#if NeedVarargsPrototypes
    _Xconst _XtString	/* name */,
    WidgetClass		/* widget_class */,
    Widget		/* parent */,
    ...
#endif
);

extern Widget XtCreateApplicationShell( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* name */,
    WidgetClass 	/* widget_class */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtAppCreateShell(
#if NeedFunctionPrototypes
    _Xconst _XtString	/* application_name */,
    _Xconst _XtString	/* application_class */,
    WidgetClass 	/* widget_class */,
    Display*		/* display */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtVaAppCreateShell(
#if NeedVarargsPrototypes
    _Xconst _XtString	/* application_name */,
    _Xconst _XtString	/* application_class */,
    WidgetClass		/* widget_class */,
    Display*		/* display */,
    ...
#endif
);

/****************************************************************
 *
 * Toolkit initialization
 *
 ****************************************************************/

extern void XtToolkitInitialize(
#if NeedFunctionPrototypes
    void
#endif
);

extern XtLanguageProc XtSetLanguageProc(
#if NeedFunctionPrototypes
    XtAppContext	/* app_context */,
    XtLanguageProc	/* proc */,
    XtPointer		/* client_data */
#endif
);

extern void XtDisplayInitialize(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    Display*		/* dpy */,
    _Xconst _XtString	/* application_name */,
    _Xconst _XtString	/* application_class */,
    XrmOptionDescRec* 	/* options */,
    Cardinal 		/* num_options */,
    int*		/* argc */,
    char**		/* argv */
#endif
);

extern Widget XtAppInitialize(
#if NeedFunctionPrototypes
    XtAppContext*	/* app_context_return */,
    _Xconst _XtString	/* application_class */,
    XrmOptionDescList 	/* options */,
    Cardinal 		/* num_options */,
    int*		/* argc_in_out */,
    String*		/* argv_in_out */,
    String*		/* fallback_resources */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern Widget XtVaAppInitialize(
#if NeedVarargsPrototypes
    XtAppContext*	/* app_context_return */,
    _Xconst _XtString	/* application_class */,
    XrmOptionDescList	/* options */,
    Cardinal		/* num_options */,
    int*		/* argc_in_out */,
    String*		/* argv_in_out */,
    String*		/* fallback_resources */,
    ...
#endif
);

extern Widget XtInitialize( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* shell_name */,
    _Xconst _XtString 	/* application_class */,
    XrmOptionDescRec* 	/* options */,
    Cardinal 		/* num_options */,
    int*		/* argc */,
    char**		/* argv */
#endif
);

extern Display *XtOpenDisplay(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString	/* display_string */,
    _Xconst _XtString	/* application_name */,
    _Xconst _XtString	/* application_class */,
    XrmOptionDescRec*	/* options */,
    Cardinal 		/* num_options */,
    int*		/* argc */,
    char**		/* argv */
#endif
);

extern XtAppContext XtCreateApplicationContext(
#if NeedFunctionPrototypes
    void
#endif
);

extern void XtAppSetFallbackResources(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    String*		/* specification_list */
#endif
);

extern void XtDestroyApplicationContext(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */
#endif
);

extern void XtInitializeWidgetClass(
#if NeedFunctionPrototypes
    WidgetClass 	/* widget_class */
#endif
);

extern XtAppContext XtWidgetToApplicationContext(
#if NeedFunctionPrototypes
    Widget 		/* widget */
#endif
);

extern XtAppContext XtDisplayToApplicationContext(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

extern XrmDatabase XtDatabase(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

extern XrmDatabase XtScreenDatabase(
#if NeedFunctionPrototypes
    Screen*		/* screen */
#endif
);

extern void XtCloseDisplay(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

extern void XtGetApplicationResources(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* base */,
    XtResourceList 	/* resources */,
    Cardinal 		/* num_resources */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaGetApplicationResources(
#if NeedVarargsPrototypes
    Widget		/* widget */,
    XtPointer		/* base */,
    XtResourceList	/* resources */,
    Cardinal		/* num_resources */,
    ...
#endif
);

extern void XtGetSubresources(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* base */,
    _Xconst _XtString 	/* name */,
    _Xconst _XtString 	/* class */,
    XtResourceList 	/* resources */,
    Cardinal 		/* num_resources */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaGetSubresources(
#if NeedVarargsPrototypes
    Widget		/* widget */,
    XtPointer		/* base */,
    _Xconst _XtString	/* name */,
    _Xconst _XtString	/* class */,
    XtResourceList	/* resources */,
    Cardinal		/* num_resources */,
    ...
#endif
);

extern void XtSetValues(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaSetValues(
#if NeedVarargsPrototypes
    Widget		/* widget */,
    ...
#endif
);

extern void XtGetValues(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaGetValues(
#if NeedVarargsPrototypes
    Widget		/* widget */,
    ...
#endif
);

extern void XtSetSubvalues(
#if NeedFunctionPrototypes
    XtPointer 		/* base */,
    XtResourceList 	/* resources */,
    Cardinal 		/* num_resources */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaSetSubvalues(
#if NeedVarargsPrototypes
    XtPointer		/* base */,
    XtResourceList	/* resources */,
    Cardinal		/* num_resources */,
    ...
#endif
);

extern void XtGetSubvalues(
#if NeedFunctionPrototypes
    XtPointer 		/* base */,
    XtResourceList 	/* resources */,
    Cardinal 		/* num_resources */,
    ArgList 		/* args */,
    Cardinal 		/* num_args */
#endif
);

extern void XtVaGetSubvalues(
#if NeedVarargsPrototypes
    XtPointer		/* base */,
    XtResourceList	/* resources */,
    Cardinal		/* num_resources */,
    ...
#endif
);

extern void XtGetResourceList(
#if NeedFunctionPrototypes
    WidgetClass 	/* widget_class */,
    XtResourceList*	/* resources_return */,
    Cardinal*		/* num_resources_return */
#endif
);

extern void XtGetConstraintResourceList(
#if NeedFunctionPrototypes
    WidgetClass 	/* widget_class */,
    XtResourceList*	/* resources_return */,
    Cardinal*		/* num_resources_return */
#endif
);

#define XtUnspecifiedPixmap	((Pixmap)2)
#define XtUnspecifiedShellInt	(-1)
#define XtUnspecifiedWindow	((Window)2)
#define XtUnspecifiedWindowGroup ((Window)3)
#define XtDefaultForeground	"XtDefaultForeground"
#define XtDefaultBackground	"XtDefaultBackground"
#define XtDefaultFont		"XtDefaultFont"
#define XtDefaultFontSet	"XtDefaultFontSet"

#if defined(CRAY) || defined(__arm)
#if __STDC__
#define XtOffset(p_type,field) _Offsetof(p_type,field)
#else
#ifdef CRAY2
#define XtOffset(p_type,field) \
	(sizeof(int)*((unsigned int)&(((p_type)NULL)->field)))

#else	/* !CRAY2 */

#define XtOffset(p_type,field) ((unsigned int)&(((p_type)NULL)->field))

#endif	/* !CRAY2 */
#endif  /* __STDC__ */
#else	/* ! (CRAY || __arm) */

#define XtOffset(p_type,field) \
	((Cardinal) (((char *) (&(((p_type)NULL)->field))) - ((char *) NULL)))

#endif /* !CRAY */

#ifdef offsetof
#define XtOffsetOf(s_type,field) offsetof(s_type,field)
#else
#define XtOffsetOf(s_type,field) XtOffset(s_type*,field)
#endif

/*************************************************************
 *
 * Error Handling
 *
 ************************************************************/

extern XtErrorMsgHandler XtAppSetErrorMsgHandler(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtErrorMsgHandler 	/* handler */
#endif
);

extern void XtSetErrorMsgHandler( /* obsolete */
#if NeedFunctionPrototypes
    XtErrorMsgHandler 	/* handler */
#endif
);

extern XtErrorMsgHandler XtAppSetWarningMsgHandler(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtErrorMsgHandler 	/* handler */
#endif
);

extern void XtSetWarningMsgHandler( /* obsolete */
#if NeedFunctionPrototypes
    XtErrorMsgHandler 	/* handler */
#endif
);

extern void XtAppErrorMsg(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString 	/* name */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* class */,
    _Xconst _XtString	/* default */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern void XtErrorMsg( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString 	/* name */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* class */,
    _Xconst _XtString	/* default */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern void XtAppWarningMsg(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString 	/* name */,
    _Xconst _XtString 	/* type */,
    _Xconst _XtString 	/* class */,
    _Xconst _XtString 	/* default */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern void XtWarningMsg( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* name */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* class */,
    _Xconst _XtString	/* default */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern XtErrorHandler XtAppSetErrorHandler(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtErrorHandler 	/* handler */
#endif
);

extern void XtSetErrorHandler( /* obsolete */
#if NeedFunctionPrototypes
    XtErrorHandler 	/* handler */
#endif
);

extern XtErrorHandler XtAppSetWarningHandler(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtErrorHandler 	/* handler */
#endif
);

extern void XtSetWarningHandler( /* obsolete */
#if NeedFunctionPrototypes
    XtErrorHandler 	/* handler */
#endif
);

extern void XtAppError(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString	/* message */
#endif
);

extern void XtError( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* message */
#endif
);

extern void XtAppWarning(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString	/* message */
#endif
);

extern void XtWarning( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* message */
#endif
);

extern XrmDatabase *XtAppGetErrorDatabase(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */
#endif
);

extern XrmDatabase *XtGetErrorDatabase( /* obsolete */
#if NeedFunctionPrototypes
    void
#endif
);

extern void XtAppGetErrorDatabaseText(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    _Xconst _XtString	/* name */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* class */,
    _Xconst _XtString 	/* default */,
    String 		/* buffer_return */,
    int 		/* nbytes */,
    XrmDatabase 	/* database */
#endif
);

extern void XtGetErrorDatabaseText( /* obsolete */
#if NeedFunctionPrototypes
    _Xconst _XtString	/* name */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* class */,
    _Xconst _XtString 	/* default */,
    String 		/* buffer_return */,
    int 		/* nbytes */
#endif
);

/****************************************************************
 *
 * Memory Management
 *
 ****************************************************************/

extern char *XtMalloc(
#if NeedFunctionPrototypes
    Cardinal 		/* size */
#endif
);

extern char *XtCalloc(
#if NeedFunctionPrototypes
    Cardinal		/* num */,
    Cardinal 		/* size */
#endif
);

extern char *XtRealloc(
#if NeedFunctionPrototypes
    char* 		/* ptr */,
    Cardinal 		/* num */
#endif
);

extern void XtFree(
#if NeedFunctionPrototypes
    char*		/* ptr */
#endif
);

#ifdef XTTRACEMEMORY

extern char *_XtMalloc( /* implementation-private */
#if NeedFunctionPrototypes
    Cardinal	/* size */,
    char *	/* file */,
    int	        /* line */
#endif		       
);

extern char *_XtRealloc( /* implementation-private */
#if NeedFunctionPrototypes
    char *	/* ptr */,
    Cardinal    /* size */,
    char *	/* file */,
    int		/* line */
#endif
);

extern char *_XtCalloc( /* implementation-private */
#if NeedFunctionPrototypes
    Cardinal	/* num */,
    Cardinal 	/* size */,
    char *	/* file */,
    int		/* line */
#endif
);

extern void _XtFree( /* implementation-private */
#if NeedFunctionPrototypes
    char *	/* ptr */
#endif
);

#define XtMalloc(size) _XtMalloc(size, __FILE__, __LINE__)
#define XtRealloc(ptr,size) _XtRealloc(ptr, size, __FILE__, __LINE__)
#define XtCalloc(num,size) _XtCalloc(num, size, __FILE__, __LINE__)
#define XtFree(ptr) _XtFree(ptr)

#endif /* ifdef XTTRACEMEMORY */

#define XtNew(type) ((type *) XtMalloc((unsigned) sizeof(type)))
#define XtNewString(str) \
    ((str) != NULL ? (strcpy(XtMalloc((unsigned)strlen(str) + 1), str)) : NULL)

/*************************************************************
 *
 *  Work procs
 *
 **************************************************************/

extern XtWorkProcId XtAddWorkProc( /* obsolete */
#if NeedFunctionPrototypes
    XtWorkProc 		/* proc */,
    XtPointer 		/* closure */
#endif
);

extern XtWorkProcId XtAppAddWorkProc(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    XtWorkProc 		/* proc */,
    XtPointer 		/* closure */
#endif
);

extern void  XtRemoveWorkProc(
#if NeedFunctionPrototypes
    XtWorkProcId 	/* id */
#endif
);


/****************************************************************
 *
 * Graphic Context Management
 *****************************************************************/

extern GC XtGetGC(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtGCMask 		/* valueMask */,
    XGCValues* 		/* values */
#endif
);

extern GC XtAllocateGC(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Cardinal		/* depth */,
    XtGCMask 		/* valueMask */,
    XGCValues* 		/* values */,
    XtGCMask		/* dynamicMask */,
    XtGCMask		/* unusedMask */
#endif
);

/* This implementation of XtDestroyGC differs from the formal specification
 * for historic backwards compatibility reasons.  As other implementations
 * may conform to the spec, use of XtReleaseGC is strongly encouraged.
 */
extern void XtDestroyGC( /* obsolete */
#if NeedFunctionPrototypes
    GC 			/* gc */
#endif
);

extern void XtReleaseGC(
#if NeedFunctionPrototypes
    Widget 		/* object */,
    GC 			/* gc */
#endif
);



extern void XtAppReleaseCacheRefs(
#if NeedFunctionPrototypes
    XtAppContext	/* app_context */,
    XtCacheRef*		/* cache_ref */
#endif
);

extern void XtCallbackReleaseCacheRef(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,	/* XtCacheRef */
    XtPointer 		/* call_data */
#endif
);

extern void XtCallbackReleaseCacheRefList(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    XtPointer 		/* closure */,	/* XtCacheRef* */
    XtPointer 		/* call_data */
#endif
);

extern void XtSetWMColormapWindows(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Widget*		/* list */,
    Cardinal		/* count */
#endif
);

extern String XtFindFile(
#if NeedFunctionPrototypes
    _Xconst _XtString	/* path */,
    Substitution	/* substitutions */,
    Cardinal 		/* num_substitutions */,
    XtFilePredicate	/* predicate */
#endif
);

extern String XtResolvePathname(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    _Xconst _XtString	/* type */,
    _Xconst _XtString	/* filename */,
    _Xconst _XtString	/* suffix */,
    _Xconst _XtString	/* path */,
    Substitution	/* substitutions */,
    Cardinal		/* num_substitutions */,
    XtFilePredicate 	/* predicate */
#endif
);

/****************************************************************
 *
 * Selections
 *
 *****************************************************************/

#define XT_CONVERT_FAIL (Atom)0x80000001
    
extern void XtDisownSelection(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Time 		/* time */
#endif
);

extern void XtGetSelectionValue(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Atom 		/* target */,
    XtSelectionCallbackProc /* callback */,
    XtPointer 		/* closure */,
    Time 		/* time */
#endif
);

extern void XtGetSelectionValues(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Atom*		/* targets */,
    int 		/* count */,
    XtSelectionCallbackProc /* callback */,
    XtPointer*		/* closures */,
    Time 		/* time */
#endif
);

extern void XtAppSetSelectionTimeout(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */,
    unsigned long 	/* timeout */
#endif
);

extern void XtSetSelectionTimeout( /* obsolete */
#if NeedFunctionPrototypes
    unsigned long 	/* timeout */
#endif
);

extern unsigned long XtAppGetSelectionTimeout(
#if NeedFunctionPrototypes
    XtAppContext 	/* app_context */
#endif
);

extern unsigned long XtGetSelectionTimeout( /* obsolete */
#if NeedFunctionPrototypes
    void
#endif
);

extern XSelectionRequestEvent *XtGetSelectionRequest(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    XtRequestId 	/* request_id */
#endif
);

extern void XtGetSelectionValueIncremental(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Atom 		/* target */,
    XtSelectionCallbackProc /* selection_callback */,
    XtPointer 		/* client_data */,
    Time 		/* time */
#endif
);

extern void XtGetSelectionValuesIncremental(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Atom 		/* selection */,
    Atom*		/* targets */,
    int 		/* count */,
    XtSelectionCallbackProc /* callback */,
    XtPointer*		/* client_data */,
    Time 		/* time */
#endif
);

extern void XtGrabKey(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtKeyCode 		/* keycode */,
    Modifiers	 	/* modifiers */,
    _XtBoolean 		/* owner_events */,
    int 		/* pointer_mode */,
    int 		/* keyboard_mode */
#endif
);

extern void XtUngrabKey(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtKeyCode 		/* keycode */,
    Modifiers	 	/* modifiers */
#endif
);

extern int XtGrabKeyboard(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtBoolean 		/* owner_events */,
    int 		/* pointer_mode */,
    int 		/* keyboard_mode */,
    Time 		/* time */
#endif
);

extern void XtUngrabKeyboard(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Time 		/* time */
#endif
);

extern void XtGrabButton(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    int 		/* button */,
    Modifiers	 	/* modifiers */,
    _XtBoolean 		/* owner_events */,
    unsigned int	/* event_mask */,
    int 		/* pointer_mode */,
    int 		/* keyboard_mode */,
    Window 		/* confine_to */,
    Cursor 		/* cursor */
#endif
);

extern void XtUngrabButton(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    unsigned int	/* button */,
    Modifiers	 	/* modifiers */
#endif
);

extern int XtGrabPointer(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    _XtBoolean 		/* owner_events */,
    unsigned int	/* event_mask */,
    int 		/* pointer_mode */,
    int 		/* keyboard_mode */,
    Window 		/* confine_to */,
    Cursor 		/* cursor */,
    Time 		/* time */
#endif
);

extern void XtUngrabPointer(
#if NeedFunctionPrototypes
    Widget 		/* widget */,
    Time 		/* time */
#endif
);

extern void XtGetApplicationNameAndClass(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    String*		/* name_return */,
    String*		/* class_return */
#endif
);


/*
 *	Predefined Resource Converters
 */


/* String converters */

extern Boolean XtCvtStringToAcceleratorTable(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToAtom(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Display */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToBoolean(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToBool(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToCursor(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Display */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToDimension(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToDisplay(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToFile(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToFloat(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToFont(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Display */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToFontSet(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Display, locale */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToFontStruct(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Display */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToInt(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToInitialState(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToPixel(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Screen, Colormap */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

#define XtCvtStringToPosition XtCvtStringToShort

extern Boolean XtCvtStringToShort(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToTranslationTable(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToUnsignedChar(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtStringToVisual(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Screen, depth */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

/* int converters */

extern Boolean XtCvtIntToBoolean(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToBool(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToColor(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* Screen, Colormap */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

#define XtCvtIntToDimension XtCvtIntToShort

extern Boolean XtCvtIntToFloat(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToFont(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToPixel(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToPixmap(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

#define XtCvtIntToPosition XtCvtIntToShort

extern Boolean XtCvtIntToShort(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

extern Boolean XtCvtIntToUnsignedChar(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

/* Color converter */

extern Boolean XtCvtColorToPixel(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr /* args */,	/* none */
    Cardinal*   /* num_args */,	
    XrmValuePtr	/* fromVal */,
    XrmValuePtr	/* toVal */,
    XtPointer*	/* closure_ret */
#endif
);

/* Pixel converter */

#define XtCvtPixelToColor XtCvtIntToColor


_XFUNCPROTOEND

#endif /*_XtIntrinsic_h*/
/* DON'T ADD STUFF AFTER THIS #endif */
