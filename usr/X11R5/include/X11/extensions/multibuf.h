/*
 * $XConsortium: multibuf.h,v 1.15 91/07/12 10:20:42 rws Exp $
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

#ifndef _MULTIBUF_H_
#define _MULTIBUF_H_

#include <X11/Xfuncproto.h>

#define MULTIBUFFER_PROTOCOL_NAME "Multi-Buffering"

#define MULTIBUFFER_MAJOR_VERSION	1	/* current version numbers */
#define MULTIBUFFER_MINOR_VERSION	0

#define X_MbufGetBufferVersion		0
#define X_MbufCreateImageBuffers	1
#define X_MbufDestroyImageBuffers	2
#define X_MbufDisplayImageBuffers	3
#define X_MbufSetMBufferAttributes	4
#define X_MbufGetMBufferAttributes	5
#define X_MbufSetBufferAttributes	6
#define X_MbufGetBufferAttributes	7
#define X_MbufGetBufferInfo		8
#define X_MbufCreateStereoWindow	9

/*
 * update_action field
 */
#define MultibufferUpdateActionUndefined	0
#define MultibufferUpdateActionBackground	1
#define MultibufferUpdateActionUntouched	2
#define MultibufferUpdateActionCopied		3

/*
 * update_hint field
 */
#define MultibufferUpdateHintFrequent		0
#define MultibufferUpdateHintIntermittent	1
#define MultibufferUpdateHintStatic		2

/*
 * valuemask fields
 */
#define MultibufferWindowUpdateHint	(1L << 0)
#define MultibufferBufferEventMask	(1L << 0)

/*
 * mono vs. stereo and left vs. right
 */
#define MultibufferModeMono		0
#define MultibufferModeStereo		1
#define MultibufferSideMono		0
#define MultibufferSideLeft	  	1
#define MultibufferSideRight		2

/*
 * clobber state
 */
#define MultibufferUnclobbered		0
#define MultibufferPartiallyClobbered	1
#define MultibufferFullyClobbered	2

/*
 * event stuff
 */
#define MultibufferClobberNotifyMask	0x02000000
#define MultibufferUpdateNotifyMask	0x04000000

#define MultibufferClobberNotify	0
#define MultibufferUpdateNotify		1
#define MultibufferNumberEvents		(MultibufferUpdateNotify + 1)

#define MultibufferBadBuffer		0
#define MultibufferNumberErrors		(MultibufferBadBuffer + 1)


#ifndef _MULTIBUF_SERVER_
/*
 * Extra definitions that will only be needed in the client
 */
typedef XID Multibuffer;

typedef struct {
    int	type;		    /* of event */
    unsigned long serial;   /* # of last request processed by server */
    int send_event;	    /* true if this came frome a SendEvent request */
    Display *display;	    /* Display the event was read from */
    Multibuffer buffer;	    /* buffer of event */
    int	state;		    /* see Clobbered constants above */
} XmbufClobberNotifyEvent;

typedef struct {
    int	type;		    /* of event */
    unsigned long serial;   /* # of last request processed by server */
    int send_event;	    /* true if this came frome a SendEvent request */
    Display *display;	    /* Display the event was read from */
    Multibuffer buffer;	    /* buffer of event */
} XmbufUpdateNotifyEvent;


/*
 * per-window attributes that can be got
 */
typedef struct {
    int displayed_index;	/* which buffer is being displayed */
    int update_action;		/* Undefined, Background, Untouched, Copied */
    int update_hint;		/* Frequent, Intermittent, Static */
    int window_mode;		/* Mono, Stereo */
    int nbuffers;		/* Number of buffers */
    Multibuffer *buffers;	/* Buffers */
} XmbufWindowAttributes;

/*
 * per-window attributes that can be set
 */
typedef struct {
    int update_hint;		/* Frequent, Intermittent, Static */
} XmbufSetWindowAttributes;


/*
 * per-buffer attributes that can be got
 */
typedef struct {
    Window window;		/* which window this belongs to */
    unsigned long event_mask;	/* events that have been selected */
    int buffer_index;		/* which buffer is this */
    int side;			/* Mono, Left, Right */
} XmbufBufferAttributes;

/*
 * per-buffer attributes that can be set
 */
typedef struct {
    unsigned long event_mask;	/* events that have been selected */
} XmbufSetBufferAttributes;


/*
 * per-screen buffer info (there will be lists of them)
 */
typedef struct {
    VisualID visualid;		/* visual usuable at this depth */
    int max_buffers;		/* most buffers for this visual */
    int depth;			/* depth of buffers to be created */
} XmbufBufferInfo;

_XFUNCPROTOBEGIN

extern Bool XmbufQueryExtension(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int*		/* event_base_return */,
    int*		/* error_base_return */
#endif
);

extern Status XmbufGetVersion(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int*		/* major_version_return */,
    int*		/* minor_version_return */
#endif
);

extern int XmbufCreateBuffers(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    Window		/* w */,
    int			/* count */,
    int			/* update_action */,
    int			/* update_hint */,
    Multibuffer*	/* buffers */
#endif
);

extern void XmbufDestroyBuffers(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    Window		/* window */
#endif
);

extern void XmbufDisplayBuffers(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int			/* count */,
    Multibuffer*	/* buffers */,
    int			/* min_delay */,
    int			/* max_delay */
#endif
);

extern Status XmbufGetWindowAttributes(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Window			/* w */,
    XmbufWindowAttributes*	/* attr */
#endif
);

extern void XmbufChangeWindowAttributes(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Window			/* w */,
    unsigned long		/* valuemask */,
    XmbufSetWindowAttributes*	/* attr */
#endif
);

extern Status XmbufGetBufferAttributes(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Multibuffer			/* b */,
    XmbufBufferAttributes*	/* attr */
#endif
);

extern void XmbufChangeBufferAttributes(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Multibuffer			/* b */,
    unsigned long		/* valuemask */,
    XmbufSetBufferAttributes*	/* attr */
#endif
);

extern Status XmbufGetScreenInfo(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Drawable			/* d */,
    int*			/* nmono_return */,
    XmbufBufferInfo**		/* mono_info_return */,
    int*			/* nstereo_return */,
    XmbufBufferInfo**		/* stereo_info_return */
#endif
);

extern Window XmbufCreateStereoWindow(
#if NeedFunctionPrototypes
    Display*			/* dpy */,
    Window			/* parent */,
    int				/* x */,
    int				/* y */,
    unsigned int		/* width */,
    unsigned int		/* height */,
    unsigned int		/* border_width */,
    int				/* depth */,
    unsigned int		/* class */,
    Visual*			/* visual */,
    unsigned long		/* valuemask */,
    XSetWindowAttributes*	/* attr */,
    Multibuffer*		/* leftp */,
    Multibuffer*		/* rightp */
#endif
);

_XFUNCPROTOEND

#endif /* _MULTIBUF_SERVER_ */
#endif /* _MULTIBUF_H_ */
