/*
 * $XConsortium: Xi18nint.h,v 1.22 92/10/22 14:26:26 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * OMRON AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OMRON OR MIT BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 *	Author:	Seiji Kuwari	OMRON Corporation
 *				kuwa@omron.co.jp
 *				kuwa%omron.co.jp@uunet.uu.net
 */				

#ifndef	_XI18NINT_H_
#define	_XI18NINT_H_

#include "Xlocaleint.h"

/* we should create a data part, but it would affect too much existing code */
typedef struct {
    XFontSetMethods	methods;	/* methods of this font set */
    XFontSetCoreRec	core;		/* core data of this font set */
    Display		*display;	/* display */
    XLocale		xlc;
    int			*ctid;
    XCharStruct		min_bounds;	/* min bounds of all fonts */
    XCharStruct		max_bounds;	/* max bounds of all fonts */
    int			ascent;		/* font set ascent */
    int			descent;	/* font set descent */
} XsiFontSetRec, *XsiFontSet;

#ifdef XML
#define XNQueryLanguage		"queryLanguage"
#define XNUsingLanguage		"usingLanguage"
#define XNCurrentLanguage	"currentLanguage"
#define XNChangeLocaleCB	"changeLocaleCB"
#endif

typedef struct _ICValues {
    char	*res_name;
    char	*res_class;
#ifdef XML
    char	*using_language;
    char	*current_language;
    int		(*ch_locale_cb)();
#endif
} ICValues;

typedef struct _XIMQueue {
    short type;				/* 0: XEvent, 1: String		 */
    short string_is_mb;			/* 0: CT, 1: MB			 */
    int length;				/* length of String or XEvent    */
    KeySym keysym;			/* keysym			 */
    unsigned int state;			/* state 			 */
    int buf_max;			/* max of buffer		 */
    char *ptr;				/* a pointer of String or XEvent */
    struct _XIMQueue *next;
} XIMQueue;

#ifndef	NO_LOCAL_IM

#define BITSIZ (sizeof(int) * 8)
#define CHANGE_MAX 256/BITSIZ

typedef struct {
    KeySym	keysym;
    unsigned int	state;
    unsigned char	*str;
} XipLocalKeySymTbl;

typedef struct _XipLocalNestedKeySym {
    KeySym keysym;
    struct _XipLocalNestedKeySym *next;
} XipLocalNestedKeySym;

typedef struct {
    XipLocalKeySymTbl	*fromkey;
    Bool	com;
    union {
	XipLocalKeySymTbl	*tokey;
	int			(*func)();
    } to;
} XipLocalCvtTbl;

typedef struct {
    int		cnt;
    XipLocalKeySymTbl	*buf;
    int		buf_cnt;
    int		nmax;
    int		check_flg[CHANGE_MAX];
    Bool	off;
    KeySym	bs;
    XipLocalCvtTbl *tbl, off_tbl;
    XipLocalNestedKeySym *no_filter;
} XipLocalCvt;

#endif	/* !NO_LOCAL_IM */

/* we should create a data part, but it would affect too much existing code */
typedef struct {
    XICMethods		methods;		/* method list of this IC */
    XICCoreRec		core;			/* core data of this IC */
    struct _XIMQueue	*in, *out, *prev, overflow;
    Bool		(*prototype_filter)();
    XLocale		mb;
    XLocale		wc;
    XID			icid;
    ICValues		values;
    char		*ct_buf;
    wchar_t		*wc_buf;
    int			max_of_ct;
    int			max_of_wc;
#ifdef XML
    XLocale		*mb_temp;
    XLocale		*wc_temp;
    int			xlc_num;
#endif
} XipICRec, *XipIC;

#ifndef	NO_LOCAL_IM
typedef struct {
    XICMethods		methods;		/* method list of this IC */
    XICCoreRec		core;			/* core data of this IC */
    struct _XIMQueue	*in, *out, *prev, overflow;
    Bool		(*prototype_filter)();
    XLocale		mb;
    XLocale		wc;
    XipLocalCvt		*xcvt;
    ICValues		values;
#ifdef XML
    XLocale		*mb_temp;
    XLocale		*wc_temp;
    XipLocalCvt		**xcvt_temp;
    int			xlc_num;
#endif
} XipLocalICRec, *XipLocalIC;
#endif	/* !NO_LOCAL_IM */

#ifdef	XML
#define ICUsingLanguage		28
#define ICCurrentLanguage	29
#define ICChangeLocaleCB	30
#define ICAllMask		0x73ffffff
#else	/* XML */
#define	ICAllMask		0x03ffffff
#endif	/* XML */

#define	StatusOffset		11


/*********************************************************************
 *
 * IM XIM structure;
 *
 *********************************************************************/

typedef struct _IMValues {
    XIMStyles	input_styles;
    char	*supported_language;
} IMValues;

#define	SEND_BUF_SZ	128
#define	RECV_BUF_SZ	128

/* we should create a data part, but it would affect too much existing code */
typedef struct {
    XIMMethods		methods;		/* method list of this IM */
    XIMCoreRec		core;			/* core data of this IM */
    XLocale		xlc;
    int			fd;			/* file discripter of socket */
    XipIC		default_ic;		/* default input context */
    char 		*client_data;
    unsigned long	default_mask;
    XIMrmResourceList	resources;
    int			num_resources;
    long		major_version;		/* major version of protocol */
    long		minor_version;		/* minor version of protocol */
    IMValues		values;
    char		send_buf[SEND_BUF_SZ];
    char		recv_buf[RECV_BUF_SZ];
    int			sp;
    int			rp;
    int			rc;
} XipIMRec, *XipIM;

#ifndef	NO_LOCAL_IM
typedef struct {
    XIMMethods		methods;		/* method list of this IM */
    XIMCoreRec		core;			/* core data of this IM */
    XLocale		xlc;
    XipLocalCvt		*xcvt;
    XIMrmResourceList	resources;
    int			num_resources;
    IMValues		values;
} XipLocalIMRec, *XipLocalIM;
#endif	/* !NO_LOCAL_IM */

#ifdef XML
#define	IMQueryLanguage		1
#endif

_XFUNCPROTOBEGIN

extern XFontSet _XsiCreateFontSet(
#if NeedFunctionPrototypes
	XLCd, Display*, char*, char**, int, char***, int*
#endif
);

extern XIM _XipOpenIM(
#if NeedFunctionPrototypes
	XLCd, Display*, XrmDatabase, char*, char*
#endif
);

extern XFontStruct *_XsiQueryFontSetFromId(
#if NeedFunctionPrototypes
	XFontSet, int
#endif
);

#ifdef XML
extern void _XsiQueryFontSetWithName(
#if NeedFunctionPrototypes
	XFontSet, char*
#endif
);
#endif

extern int _XsimbTextEscapement(
#if NeedFunctionPrototypes
	XFontSet, char*, int
#endif
);

extern int _Xsimb8TextEscapement(
#if NeedFunctionPrototypes
	XFontSet, char*, int
#endif
);

extern int _XsimbTextExtents(
#if NeedFunctionPrototypes
	XFontSet, char*, int, XRectangle*, XRectangle*
#endif
);

extern int _Xsimb8TextExtents(
#if NeedFunctionPrototypes
	XFontSet, char*, int, XRectangle*, XRectangle*
#endif
);

extern Status _XsimbTextPerCharExtents(
#if NeedFunctionPrototypes
	XFontSet, char*, int, XRectangle*, XRectangle*, int, int*,
	XRectangle*, XRectangle*
#endif
);

extern int _XsimbDrawString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, char*, int
#endif
);

extern int _Xsimb8DrawString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, char*, int
#endif
);

extern void _XsimbDrawImageString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, char*, int
#endif
);

extern void _Xsimb8DrawImageString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, char*, int
#endif
);

extern int _XsiwcTextEscapement(
#if NeedFunctionPrototypes
	XFontSet, wchar_t*, int
#endif
);

extern int _XsiwcTextExtents(
#if NeedFunctionPrototypes
	XFontSet, wchar_t*, int, XRectangle*, XRectangle*
#endif
);

extern Status _XsiwcTextPerCharExtents(
#if NeedFunctionPrototypes
	XFontSet, wchar_t*, int, XRectangle*, XRectangle*, int, int*,
	XRectangle*, XRectangle*
#endif
);

extern int _XsiwcDrawString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, wchar_t*, int
#endif
);

extern void _XsiwcDrawImageString(
#if NeedFunctionPrototypes
	Display*, Drawable, XFontSet, GC, int, int, wchar_t*, int
#endif
);

extern char *_XipGetIMValues(
#if NeedFunctionPrototypes
	XIM, XIMArg*
#endif
);

extern XIC _XipCreateIC(
#if NeedFunctionPrototypes
	XIM, XIMArg*
#endif
);

extern void _XipDestroyIC(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern void _XipSetICFocus(
#if NeedFunctionPrototypes
	XIC
#endif
);

void _XipUnsetICFocus(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern char* _XipSetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg*
#endif
);

extern char* _XipGetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg*
#endif
);

extern char* _XipmbResetIC(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern wchar_t* _XipwcResetIC(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern int _XipmbLookupString(
#if NeedFunctionPrototypes
	XIC, XKeyEvent*, char*, int, KeySym*, Status*
#endif
);

extern int _XipwcLookupString(
#if NeedFunctionPrototypes
	XIC, XKeyEvent*, wchar_t*, int, KeySym*, Status*
#endif
);

extern unsigned long _XipReadRdb(
#if NeedFunctionPrototypes
	Display*, XipIC, unsigned long, XrmDatabase, char*, char*
#endif
);

#ifndef	NO_LOCAL_IM
extern XIM _XipLocalOpenIM(
#if NeedFunctionPrototypes
	XLCd, Display*, XrmDatabase, char*, char*
#endif
);

extern char *_XipLocalGetIMValues(
#if NeedFunctionPrototypes
	XIM, XIMArg*
#endif
);

extern XIC _XipLocalCreateIC(
#if NeedFunctionPrototypes
	XIM, XIMArg*
#endif
);

extern void _XipLocalDestroyIC(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern void _XipLocalSetICFocus(
#if NeedFunctionPrototypes
	XIC
#endif
);

void _XipLocalUnsetICFocus(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern char* _XipLocalSetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg*
#endif
);

extern char* _XipLocalGetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg*
#endif
);

extern char* _XipLocalmbResetIC(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern wchar_t* _XipLocalwcResetIC(
#if NeedFunctionPrototypes
	XIC
#endif
);
#endif	/* !NO_LOCAL_IM */

_XFUNCPROTOEND

#endif	/* _XI18NINT_H_ */
