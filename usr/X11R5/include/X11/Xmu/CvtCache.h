/* $XConsortium: CvtCache.h,v 1.6 91/07/22 23:45:42 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
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
 * 
 *			       Public Interfaces
 * 
 * XmuCvtCache *XmuCvtCacheLookupDisplay (dpy)
 *     Display *dpy;
 */

#ifndef _XMU_CVTCACHE_H_
#define _XMU_CVTCACHE_H_

#include <X11/Xmu/DisplayQue.h>
#include <X11/Xfuncproto.h>

typedef struct _XmuCvtCache {
    struct {
	char **bitmapFilePath;
    } string_to_bitmap;
    /* add other per-display data that needs to be cached */
} XmuCvtCache;

_XFUNCPROTOBEGIN

extern XmuCvtCache *_XmuCCLookupDisplay(
#if NeedFunctionPrototypes
    Display*	/* dpy */
#endif
);

_XFUNCPROTOEND

#endif /* _XMU_CVTCACHE_H_ */
