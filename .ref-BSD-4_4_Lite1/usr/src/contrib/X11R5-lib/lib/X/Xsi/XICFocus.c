/*
 * $XConsortium: XICFocus.c,v 1.12 91/06/05 09:24:13 rws Exp $
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

#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"

/*
 * Require the input manager to focus the focus window attached to the ic
 * argument.
 */
void
_XipSetICFocus(supic)
    XIC supic;
{
    XipIC		ic = (XipIC)supic;
    XipIM		im = ipIMofIC(ic);
    ximICFocusReq	req;
    ximEventReply	reply;

    if (im->fd < 0) {
	return;
    }
    _XRegisterFilterByMask(im->core.display, ic->core.focus_window,
			   KeyPressMask,
			   ic->prototype_filter, (XPointer)ic);
    req.reqType = XIM_SetICFocus;
    req.length = sz_ximICFocusReq;
    req.xic = ic->icid;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximICFocusReq) >= 0) &&
	(_XipFlushToIM(im) >= 0)) {
	for (;;) {
	    if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0) ||
		(reply.state == 0xffff)) {
		return;
	    }
	    if (reply.detail == XIM_CALLBACK) {
		if (_XipCallCallbacks(ic) < 0) {
		    return;
		}
	    } else {
		return;
	    }
	}
    }
}

/*
 * Require the input manager to unfocus the focus window attached to the ic
 * argument.
 */
void
_XipUnsetICFocus(supic)
    XIC supic;
{
    XipIC		ic = (XipIC)supic;
    XipIM		im = ipIMofIC(ic);
    ximICFocusReq	req;
    ximEventReply	reply;

    if (im->fd < 0) {
	return;
    }
    _XUnregisterFilter(im->core.display, ic->core.focus_window,
		       ic->prototype_filter, (XPointer)ic);
    req.reqType = XIM_UnsetICFocus;
    req.length = sz_ximICFocusReq;
    req.xic = ic->icid;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximICFocusReq) >= 0) &&
	(_XipFlushToIM(im) >= 0)) {
	for (;;) {
	    if ((_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0) ||
		(reply.state == 0xffff)) {
		return;
	    }
	    if (reply.detail == XIM_CALLBACK) {
		if (_XipCallCallbacks(ic) < 0) {
		    return;
		}
	    } else {
		return;
	    }
	}
    }
}
