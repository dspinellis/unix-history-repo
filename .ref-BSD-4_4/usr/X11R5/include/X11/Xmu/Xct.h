#ifndef _Xct_h
#define _Xct_h

/* 
 * $XConsortium: Xct.h,v 1.6 91/07/22 23:46:25 converse Exp $
 * Copyright 1989 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

#include <X11/Xfuncproto.h>

#define XctVersion 1

typedef unsigned char *XctString;

typedef enum {
    XctUnspecified,
    XctLeftToRight,
    XctRightToLeft
} XctHDirection;

typedef unsigned long XctFlags;

/* These are bits in XctFlags. */

#define XctSingleSetSegments	0x0001
   /* This means that returned segments should contain characters from only
    * one set (C0, C1, GL, GR).  When this is requested, XctSegment is never
    * returned, instead XctC0Segment, XctC1Segment, XctGlSegment, and
    * XctGRSegment are returned.  C0 and C1 segments are always returned as
    * singleton characters.
    */

#define XctProvideExtensions	0x0002
   /* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then syntactically correct but unknown
    * control sequences should be returned as XctExtension items.  If this
    * flag is not set, and the Compound Text string version indicates that
    * extensions cannot be ignored, then each unknown control sequence will be
    * reported as an XctError.
    */

#define XctAcceptC0Extensions	0x0004
   /* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then unknown C0 characters should be
    * treated as if they were legal, and returned as C0 characters (regardless
    * of how XctProvideExtensions is set).  If this flag is not set, then all
    * unknown C0 characters are treated according to XctProvideExtensions.
    */

#define XctAcceptC1Extensions	0x0008
   /* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then unknown C0 characters should be
    * treated as if they were legal, and returned as C0 characters (regardless
    * of how XctProvideExtensions is set).  If this flag is not set, then all
    * unknown C0 characters are treated according to XctProvideExtensions.
    */

#define XctHideDirection	0x0010
   /* This means that horizontal direction changes should be reported as
    * XctHorizontal items.  If this flag is not set, then direction changes are
    * not returned as items, but the current direction is still maintained and
    * reported for other items.
    */

#define XctFreeString		0x0020
   /* This means that XctFree should free the Compound Text string (that was
    * passed to XctCreate.  If this flag is not set, the string is not freed.
    */

#define XctShiftMultiGRToGL	0x0040
   /* Translate GR segments on-the-fly into GL segments for the GR sets:
    * GB2312.1980-1, JISX0208.1983-1, and KSC5601.1987-1.
    */

/* This is the return type for XctNextItem. */
typedef enum {
    XctSegment,		/* used when XctSingleSetSegments is not requested */
    XctC0Segment,	/* used when XctSingleSetSegments is requested */
    XctGLSegment,	/* used when XctSingleSetSegments is requested */
    XctC1Segment,	/* used when XctSingleSetSegments is requested */
    XctGRSegment,	/* used when XctSingleSetSegments is requested */
    XctExtendedSegment,	/* an extended segment */
    XctExtension,	/* used when XctProvideExtensions is requested */
    XctHorizontal,	/* horizontal direction or depth change */
    XctEndOfText,	/* end of text string */
    XctError		/* syntactic or semantic error */
} XctResult;

typedef struct _XctRec {
    XctString		total_string;	/* as given to XctCreate */
    int			total_length;	/* as given to XctCreate */
    XctFlags		flags;		/* as given to XctCreate */
    int			version;	/* indicates the version of the CT spec
					 * the string was produced from */
    int			can_ignore_exts;/* non-zero if ignoring extensions is
					 * acceptable, else zero */
    XctString		item;		/* item returned from XctNextItem */
    int			item_length;	/* length of item in bytes */
    int			char_size;	/* number of bytes per character in
					 * item, with zero meaning variable */
    char		*encoding;	/* Encoding name for item */
    XctHDirection	horizontal;	/* direction of item */
    int			horz_depth;	/* current direction nesting depth */
    char		*GL;		/* "{I} F" string for current GL */
    char		*GL_encoding;	/* Encoding name for current GL */
    int			GL_set_size;	/* 94 or 96 */
    int			GL_char_size;	/* number of bytes per GL character */
    char		*GR;		/* "{I} F" string for current GR */
    char		*GR_encoding;	/* Encoding name for current GR */
    int			GR_set_size;	/* 94 or 96 */
    int			GR_char_size;	/* number of bytes per GR character */
    char		*GLGR_encoding;	/* Encoding name for current GL+GR,
					 * if known */
    struct _XctPriv	*priv;		/* private to parser, don't peek */
} *XctData;

/* these are the external routines */
_XFUNCPROTOBEGIN

extern XctData XctCreate(
#if NeedFunctionPrototypes
    _Xconst unsigned char *	/* string */,
    int			/* length */,
    XctFlags		/* flags */
#endif
);

extern XctResult XctNextItem(
#if NeedFunctionPrototypes
    XctData	/* data */
#endif
);

extern void XctFree(
#if NeedFunctionPrototypes
    XctData	/* data */
#endif
);

extern void XctReset(
#if NeedFunctionPrototypes
    XctData	/* data */
#endif
);

_XFUNCPROTOEND

#endif /* _Xct_h */
