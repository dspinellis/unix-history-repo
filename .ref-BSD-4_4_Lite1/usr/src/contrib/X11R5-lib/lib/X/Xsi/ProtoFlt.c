/*
 * $XConsortium: ProtoFlt.c,v 1.28 92/10/22 14:25:16 rws Exp $
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
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"


static void
_call_preedit_draw(ic, cb)
    XipIC ic;
    XIMCallback *cb;
{
    XipIM	im = ipIMofIC(ic);
    XIMPreeditDrawCallbackStruct	call_data;
    XIMText		text;
    ximPreDrawReply	reply;
    XIMFeedback		fb;
    int		ret;
    int		wc_len;
    int		length;
    int		scanned_bytes;
#ifndef X_WCHAR
    int		mb_len;
    char	*mbuf;
#endif

    if (_XipReadFromIM(im, (char *)&reply, sz_ximPreDrawReply) < 0) {
	return;
    }
    length = (int)reply.length;
    if (length > 0) {
	if (reply.encoding_is_wchar != True) {
	    if (length > ic->max_of_ct) {
		if (ic->ct_buf == NULL) {
		    ic->ct_buf = (char *)Xmalloc((ic->max_of_ct = length + 1));
		} else {
		    ic->ct_buf = (char *)Xrealloc(ic->ct_buf,
						     (ic->max_of_ct = length + 1));
		}
	    }
	    if (_XipReadFromIM(im, ic->ct_buf, length) < 0) return;
	    ic->ct_buf[length] = 0;
	}
	if (length > ic->max_of_wc) {
	    if (ic->wc_buf == NULL) {
		ic->wc_buf = (wchar_t *)Xmalloc(
				(ic->max_of_wc = length + 1) * sizeof(wchar_t));
	    } else {
		ic->wc_buf = (wchar_t *)Xrealloc(ic->wc_buf,
				(ic->max_of_wc = length + 1) * sizeof(wchar_t));
	    }
	}
	if (reply.encoding_is_wchar != True) {
	    wc_len = ic->max_of_wc;
#ifdef X_WCHAR
	    ret = _XConvertCTToWC(ic->wc, (unsigned char *)ic->ct_buf, length,
				  (wchar *)ic->wc_buf, &wc_len, &scanned_bytes,
				  (_State *)NULL);
#else
	    mb_len = length * 2;
	    mbuf = _XAllocScratch(im->core.display, mb_len);
	    ret = _XConvertCTToMB(ic->mb, (unsigned char *)ic->ct_buf, length,
				  mbuf, &mb_len, &scanned_bytes,
				  (_State *)NULL);
	    if (ret >= 0) {
#ifdef macII
		wc_len = 0;
#else
		mbuf[mb_len] = '\0';
		wc_len = mbstowcs(ic->wc_buf, mbuf, ic->max_of_wc);
#endif
		if (wc_len == ic->max_of_wc)
		    return;
	    }
#endif
	    if (ret < 0) {
		return;
	    }
	} else {
	    if (_XipReadFromIM(im, (char *)ic->wc_buf,
			       (length * sizeof(wchar_t))) < 0) return;
	    ic->wc_buf[length] = 0;
	    wc_len = length;
	}
	text.length = wc_len;
	fb = (XIMFeedback)reply.feedback;
	text.feedback = &fb;
	text.encoding_is_wchar = True;
	text.string.wide_char = ic->wc_buf;
    } else {
	text.length = 0;
	text.feedback = 0;
	text.encoding_is_wchar = True;
	text.string.wide_char = NULL;
	text.string.multi_byte = NULL;
    }
    call_data.text = &text;
    call_data.caret = (int)reply.caret;
    call_data.chg_first = (int)reply.chg_first;
    call_data.chg_length = (int)reply.chg_length;

    (void)(cb->callback)(ic, cb->client_data, &call_data);
    return;
}

static void
_call_preedit_caret(ic, cb)
    XipIC ic;
    XIMCallback *cb;
{
    XIMPreeditCaretCallbackStruct	call_data;
    ximPreCaretReply			reply;

    if (_XipReadFromIM(ipIMofIC(ic),
		       (char *)&reply, sz_ximPreCaretReply) < 0) {
	return;
    }
    call_data.position = (int)reply.position;
    switch((int)reply.direction) {
	case XIM_CB_FW_CHAR:
	    call_data.direction = XIMForwardChar;
	    break;
	case XIM_CB_BW_CHAR:
	    call_data.direction = XIMBackwardChar;
	    break;
	case XIM_CB_FW_WORD:
	    call_data.direction = XIMForwardWord;
	    break;
	case XIM_CB_BW_WORD:
	    call_data.direction = XIMBackwardWord;
	    break;
	case XIM_CB_CARET_UP:
	    call_data.direction = XIMCaretUp;
	    break;
	case XIM_CB_CARET_DOWN:
	    call_data.direction = XIMCaretDown;
	    break;
	case XIM_CB_NEXT_LINE:
	    call_data.direction = XIMNextLine;
	    break;
	case XIM_CB_PREV_LINE:
	    call_data.direction = XIMPreviousLine;
	    break;
	case XIM_CB_LINE_START:
	    call_data.direction = XIMLineStart;
	    break;
	case XIM_CB_LINE_END:
	    call_data.direction = XIMLineEnd;
	    break;
	case XIM_CB_ABS_POS:
	    call_data.direction = XIMAbsolutePosition;
	    break;
	case XIM_CB_DONT_CHANGE:
	    call_data.direction = XIMDontChange;
	    break;
	default:
	    return;
    }
    call_data.style = (XIMCaretStyle)reply.style;
    (void)(cb->callback)(ic, cb->client_data, &call_data);
    return;
}

static void
_call_status_draw(ic, cb)
    XipIC ic;
    XIMCallback *cb;
{
    XipIM		im = ipIMofIC(ic);
    XIMStatusDrawCallbackStruct		call_data;
    XIMText				text;
    ximStatusDrawReply			reply;
    XIMFeedback		fb;
    int			ret;
    int			wc_len = 0;
    int			length;
    int			scanned_bytes;
#ifndef X_WCHAR
    int		mb_len;
    char	*mbuf;
#endif

    if (_XipReadFromIM(im, (char *)&reply, sz_ximStatusDrawReply) < 0) {
	return;
    }
    if (reply.type == XIM_ST_BITMAP) {
	call_data.type = XIMBitmapType;
	call_data.data.bitmap = (Pixmap)reply.bitmap;
    } else {
	length = (int)reply.length;
	if (length > 0) {
	    if (reply.encoding_is_wchar != True) {
		if (length > ic->max_of_ct) {
		    if (ic->ct_buf == NULL) {
			ic->ct_buf = (char *)Xmalloc(
					    (ic->max_of_ct = length + 1));
		    } else {
			ic->ct_buf = (char *)Xrealloc(ic->ct_buf,
					      (ic->max_of_ct = length + 1));
		    }
		}
		if (_XipReadFromIM(im, ic->ct_buf, length) < 0) return;
		ic->ct_buf[length] = 0;
	    }
	    if (length > ic->max_of_wc) {
		if (ic->wc_buf == NULL) {
		    ic->wc_buf = (wchar_t *)Xmalloc(
			    (ic->max_of_wc = length + 1) * sizeof(wchar_t));
		} else {
		    ic->wc_buf = (wchar_t *)Xrealloc(ic->wc_buf,
			    (ic->max_of_wc = length + 1) * sizeof(wchar_t));
		}
	    }
	    if (reply.encoding_is_wchar != True) {
		wc_len = ic->max_of_wc;
#ifdef X_WCHAR
		ret = _XConvertCTToWC(ic->wc, (unsigned char *)ic->ct_buf,
				      length, (wchar *)ic->wc_buf, &wc_len,
				      &scanned_bytes, (_State *)NULL);
#else
		mb_len = length * 2;
		mbuf = _XAllocScratch(im->core.display, mb_len);
		ret = _XConvertCTToMB(ic->mb, (unsigned char *)ic->ct_buf,
				      length, mbuf, &mb_len, &scanned_bytes,
				      (_State *)NULL);
		if (ret >= 0) {
#ifdef macII
		    wc_len = 0;
#else
		    mbuf[mb_len] = '\0';
		    wc_len = mbstowcs(ic->wc_buf, mbuf, ic->max_of_wc);
#endif
		    if (wc_len == ic->max_of_wc)
			return;
		}
#endif
		if (ret < 0) {
		    return;
		}
	    } else {
		if (_XipReadFromIM(im, (char *)ic->wc_buf,
				   (length * sizeof(wchar_t))) < 0) return;
		ic->wc_buf[length] = 0;
		wc_len = length;
	    }
	}
	call_data.type = XIMTextType;
	call_data.data.text = &text;
	text.length = wc_len;
	fb = (XIMFeedback)reply.feedback;
	text.feedback = &fb;
	text.encoding_is_wchar = True;
	text.string.wide_char = ic->wc_buf;
    }

    (void)(cb->callback)(ic, cb->client_data, &call_data);
    return;
}

int
_XipCallCallbacks(ic)
    XipIC ic;
{
    ICCallbacks		*pre_cbs = &ic->core.preedit_attr.callbacks;
    ICCallbacks		*st_cbs = &ic->core.status_attr.callbacks;
    ximNormalReply	reply;
    int		type;

    for (;;) {
	if (_XipReadFromIM(ipIMofIC(ic),
			   (char *)&reply, sz_ximNormalReply) < 0) {
	    return(-1);
	}
	type = (int)reply.detail;
	if (type == XIM_CB_PRE_START) {
	    if (pre_cbs->start.callback) {
		(*pre_cbs->start.callback)(ic, pre_cbs->start.client_data,
					   NULL);
	    }
	} else if (type == XIM_CB_PRE_DONE) {
	    if (pre_cbs->done.callback) {
		(*pre_cbs->done.callback)(ic, pre_cbs->done.client_data, NULL);
	    }
	} else if (type == XIM_CB_PRE_DRAW) {
	    (void)_call_preedit_draw(ic, &(pre_cbs->draw));
	} else if (type == XIM_CB_PRE_CARET) {
	    (void)_call_preedit_caret(ic, &(pre_cbs->caret));
	} else if (type == XIM_CB_ST_START) {
	    if (st_cbs->start.callback) {
		(*st_cbs->start.callback)(ic, st_cbs->start.client_data, NULL);
	    }
	} else if (type == XIM_CB_ST_DONE) {
	    if (st_cbs->done.callback) {
		(*st_cbs->done.callback)(ic, st_cbs->done.client_data, NULL);
	    }
	} else if (type == XIM_CB_ST_DRAW) {
	    (void)_call_status_draw(ic, &(st_cbs->draw));
	} else {
	    return(0);
	}
    }
}

Bool
_XipBackEndFilter(display, window, ev, client_data)
    Display *display;
    Window window;
    XEvent *ev;
    XPointer client_data;
{
    register int	count;
    register XipIC	ic = (XipIC)client_data;
    XipIM		im = ipIMofIC(ic);
    ximEventReq		req;
    ximEventReply	reply;
    ximReturnReply	reply1;
    KeySym		keysym;
    int			keycode;
    int			mode;
    register int	i;
    XEvent		dummy_ev;
    int			ret;
    int			first = 0;
#ifdef	XML
    char		tmp_lc_name[32];
#endif	/* XML */

    if (!im || (im->fd < 0)) return(False);
    /*
     * If key_code is 0, this key event be regarded
     * as the event pushed back by this filter.
     */
    if (ev->xkey.keycode == 0) {
	if (_XipTypeOfNextICQueue(ic) == XIM_KEYSYM &&
	    (keysym = _XipKeySymOfNextICQueue(ic)) > XK_BackSpace){
	    if ((keycode = XKeysymToKeycode(display, keysym)) != 0) {
		ev->xkey.keycode = keycode;
		ev->xkey.state = _XipStateOfNextICQueue(ic);
		_XipFreeNextICQueue(ic);
	    }
	}
	return(False);
    }
    if (ic->out) ic->out = NULL;

    if (ev->xany.window != window) {
	ev->xany.window = window;
    }
    req.reqType = XIM_Event;
    req.length = sz_ximEventReq + sizeof(XEvent);
    req.xic = ic->icid;
    if ((_XipWriteToIM(im, (char *)&req, sz_ximEventReq) < 0)
	|| (_XipWriteToIM(im, (char *)ev, sizeof(XEvent)) < 0)
	|| (_XipFlushToIM(im) < 0)) {
	return(-1);
    }
    
    ret = True;
    for (;;) {
	if (_XipReadFromIM(im, (char *)&reply, sz_ximEventReply) < 0) {
	    return(-1);
	}
	if (reply.state != 0) {
	    return(-1);
	}
	mode = (int)reply.detail;
	if (mode == XIM_NOTHING) {
	    /*
	     * The input manager filtered a event.
	     */
	    ret = True;
	} else if (mode == XIM_NOFILTER) {
	    /*
	     * The input manager didn't filter a event.
	     */
	    ret = False;
	} else if (mode == XIM_RETURN) {
	    /*
	     * The input manager returns some strings and events.
	     */
	    count = 0;
	    for (;;) {
		if (_XipReadFromIM(im, (char *)&reply1,
				   sz_ximReturnReply) < 0) {
		    return(-1);
		}
		if (reply1.type == XIM_KEYSYM || reply1.type == XIM_STRING) {
		    if (_XipPutICQueue(ic, (short)reply1.type,
				       (int)reply1.length,
				       (KeySym)reply1.keysym, 0, 0, NULL) < 0) {
			return(-1);
		    }
		    count++;
		} else {
		    break;
		}
	    }
	    bcopy((char *)ev, (char *)&dummy_ev, sizeof(XEvent));
	    dummy_ev.type = KeyPress;
	    dummy_ev.xkey.state = 0;
	    dummy_ev.xkey.keycode = 0;
	    if (first == 0) count--;
	    for (i = 0; i < count; i++) {
		XPutBackEvent(display, &dummy_ev);
	    }
	    ret = False;
	    if (first > 0) continue;
	    first++;
	    ev->xkey.state = 0;
	    ev->xkey.keycode = 0;
	    if (_XipTypeOfNextICQueue(ic) == XIM_KEYSYM &&
		(keysym = _XipKeySymOfNextICQueue(ic)) > XK_BackSpace){
		if ((keycode = XKeysymToKeycode(display, keysym)) != 0) {
		    ev->xkey.keycode = keycode;
		    ev->xkey.state = _XipStateOfNextICQueue(ic);
		    _XipFreeNextICQueue(ic);
		}
	    }
	} else if (mode == XIM_CALLBACK) {
	    /*
	     * Calling callback routines.
	     */
	    if (_XipCallCallbacks(ic) < 0) {
		return(True);
	    }
#ifdef	XML
	} else if (mode == XIM_CH_LOCALE) {
	    if (_XipReadFromIM(im, tmp_lc_name, reply.number) < 0) {
		return(-1);
	    }
	    tmp_lc_name[reply.number] = '\0';
	    _XipChangeLocale(ic, tmp_lc_name);
	    (*ic->values.ch_locale_cb)(tmp_lc_name);
	    ret = True;
#endif	/* XML */
	} else {
	    return(ret);
	}
    }
}
