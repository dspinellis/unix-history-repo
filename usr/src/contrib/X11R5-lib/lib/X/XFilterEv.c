/*
 * $XConsortium: XFilterEv.c,v 1.8 91/06/05 09:15:44 rws Exp $
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

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xlcint.h"

#if __STDC__
#define Const const
#else
#define Const /**/
#endif
extern long Const _Xevent_to_mask[];

/*
 * Look up if there is a specified filter for the event.
 */
Bool
XFilterEvent(ev, window)
    XEvent *ev;
    Window window;
{
    XFilterEventList	p;
    Window		win;
    long		mask;
    Bool		ret;

    if (window)
	win = window;
    else
	win = ev->xany.window;
    if (ev->type >= LASTEvent)
	mask = 0;
    else
	mask = _Xevent_to_mask[ev->type];

    LockDisplay(ev->xany.display);
    for (p = ev->xany.display->im_filters; p != NULL; p = p->next) {
	if (win == p->window) {
	    if ((mask & p->event_mask) ||
		(ev->type >= p->start_type && ev->type <= p->end_type)) {
		ret = (*(p->filter))(ev->xany.display, p->window, ev,
				      p->client_data);
		UnlockDisplay(ev->xany.display);
		return(ret);
	    }
	}
    }
    UnlockDisplay(ev->xany.display);
    return(False);
}
