/*
 * $XConsortium: XIMQueue.c,v 1.12 92/10/22 14:26:15 rws Exp $
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
#include "Xi18nint.h"
#include "XIMlibint.h"
#include "XIMproto.h"

short
_XipTypeOfNextICQueue(ic)
    XipIC ic;
{
    if (ic->out == NULL) return((short)0);
    return((short)ic->out->type);
}

unsigned int
_XipStateOfNextICQueue(ic)
    XipIC ic;
{
    if (ic->out == NULL) return(0);
    return(ic->out->state);
}

KeySym
_XipKeySymOfNextICQueue(ic)
    XipIC ic;
{
    if (ic->out == NULL) return(0);
    return((KeySym)ic->out->keysym);
}

char *
_XipStringOfNextICQueue(ic)
    XipIC ic;
{
    if (ic->out == NULL || ic->out->length == 0) return(0);
    return((char *)ic->out->ptr);
}

void
_XipFreeNextICQueue(ic)
    XipIC ic;
{
    if (ic->out) {
	if (ic->out->next == ic->in) {
	    ic->out = NULL;
	} else {
	    ic->out = ic->out->next;
	}
    }
}

/*
 * Stack 
 */
int
_XipPutICQueue(ic, type, length, keysym, state, string_is_mb, str)
    XipIC ic;
    short type;
    int length;
    KeySym keysym;
    unsigned int state;
    short string_is_mb;
    unsigned char *str;
{
    XipIM im = ipIMofIC(ic);

    /*
     * If a queue of stack is NULL, allocate a structure XIMQueue.
     */
    if (ic->in == NULL) {
	if ((ic->in = (XIMQueue *)Xmalloc(sizeof(XIMQueue))) == NULL)
	    return(-1);
	ic->in->buf_max = sizeof(XEvent);
	if ((ic->in->ptr = (char *)Xmalloc(sizeof(XEvent))) == NULL) return(-1);
	ic->in->length = 0;
	ic->in->keysym = 0;
	ic->in->state = 0;
	ic->in->next  = ic->in;
	ic->prev = ic->in;
    }
    if (ic->in == ic->out) {
	if ((ic->in = (XIMQueue *)Xmalloc(sizeof(XIMQueue))) == NULL)
	    return(-1);
	ic->in->buf_max = sizeof(XEvent);
	if ((ic->in->ptr = (char *)Xmalloc(sizeof(XEvent))) == NULL) return(-1);
	ic->in->length = 0;
	ic->in->keysym = 0;
	ic->in->state = 0;
	ic->in->next = ic->out;
	ic->prev->next = ic->in;
    }

    ic->in->type = type;
    ic->in->string_is_mb = string_is_mb;
    if (type == XIM_KEYSYM) {
	if (str) {
	    length = strlen((char *)str);
	}
	ic->in->length = length;
	ic->in->keysym = keysym;
	ic->in->state = state;
	if (length > 0) {
	    if (ic->in->buf_max < length) {
		ic->in->ptr = (char *)Xrealloc((char *)ic->in->ptr,
					       (unsigned)length);
		ic->in->buf_max = length;
	    }
	    if (!str) {
		if (_XipReadFromIM(im, ic->in->ptr, ic->in->length) < 0)
		    return(-1);
	    } else {
		(void) strncpy(ic->in->ptr, (char *)str, length);
	    }
	}
    } else if (type == XIM_STRING) {
	ic->in->length = length;
	ic->in->keysym = 0;
	ic->in->state = 0;
	if (ic->in->buf_max < length) {
	    ic->in->ptr = (char *)Xrealloc((char *)ic->in->ptr,
					   (unsigned)length);
	    ic->in->buf_max = length;
	}
	if (_XipReadFromIM(im, ic->in->ptr, ic->in->length) < 0)
	    return(-1);
    }
    if (ic->out == NULL) ic->out = ic->in;
    ic->prev = ic->in;
    ic->in = ic->in->next;
    return(0);
}

/*
 * Get 
 */
void
_XipGetNextICQueue(ic, type, length, keysym, string_is_mb, ptr)
    XipIC ic;
    short *type;
    int *length;
    KeySym *keysym;
    short *string_is_mb;
    char **ptr;
{
    if (ic->out) {
	*type = ic->out->type;
	*length = ic->out->length;
	if (keysym != NULL) *keysym = ic->out->keysym;
	*string_is_mb = ic->out->string_is_mb;
	*ptr = ic->out->ptr;
	if (ic->out->next == ic->in) {
	    ic->out = NULL;
	} else {
	    ic->out = ic->out->next;
	}
    } else {
	*type = 0;
    }
}

void
_XipFreeAllICQueue(ic)
    XipIC ic;
{
    register XIMQueue *start = ic->in;
    register XIMQueue *p, *f;
    for (p = ic->in; p;) {

	if (p->buf_max > 0) Xfree((char *)p->ptr);
	f = p;
	p = p->next;
	Xfree((char *)f);
	if (p == start) break;
    }
}

/*
 * Save to Overflow Queue
 */
void
_XipSaveOverflowICQueue(ic, type, length, keysym, string_is_mb, ptr)
    XipIC ic;
    short type;
    int length;
    KeySym keysym;
    short string_is_mb;
    char *ptr;
{
    register XIMQueue *q = &ic->overflow;

    q->type = type;
    q->length = length;
    q->keysym = keysym;
    q->state = 0;
    q->string_is_mb = string_is_mb;
    if (q->length > 0) {
	if (q->buf_max < q->length) {
	    q->ptr = (char *)Xrealloc((char *)q->ptr, (unsigned)q->length);
	    q->buf_max = q->length;
	}
	(void) bcopy(ptr, q->ptr, (unsigned)q->length);
    }
}

/*
 * Get from Overflow Queue
 */
void
_XipGetOverflowICQueue(ic, type, length, keysym, string_is_mb, ptr)
    XipIC ic;
    short *type;
    int *length;
    KeySym *keysym;
    short *string_is_mb;
    char **ptr;
{
    register XIMQueue *q = &ic->overflow;

    if (q->type == XIM_STRING || q->type == XIM_KEYSYM) {
	*type = q->type;
	*length = q->length;
	if (keysym != NULL) *keysym = q->keysym;
	*string_is_mb = q->string_is_mb;
	*ptr = q->ptr;
	q->type = 0;
    } else {
	*type = 0;
    }
}
