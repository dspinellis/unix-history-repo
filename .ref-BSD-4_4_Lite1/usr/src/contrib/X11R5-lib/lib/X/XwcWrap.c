/*
 * $XConsortium: XwcWrap.c,v 11.4 91/05/01 10:57:25 rws Exp $
 */

/*
 * Copyright 1991 by the Massachusetts Institute of Technology
 * Copyright 1991 by the Open Software Foundation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Open Software Foundation and M.I.T.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Open Software
 * Foundation and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OPEN SOFTWARE FOUNDATION AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL OPEN SOFTWARE FOUNDATIONN OR M.I.T. BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 *		 M. Collins		OSF  
 */				

#include "Xlibint.h"
#include "Xlcint.h"

void
XwcDrawText(dpy, d, gc, x, y, text_items, nitems)
    Display            *dpy;
    Drawable            d;
    GC                  gc;
    int                 x, y;
    XwcTextItem        *text_items;
    int                 nitems;
{
    register XFontSet fs;
    register XwcTextItem *p = text_items;
    register int i = nitems;
    register int esc;

    /* ignore leading items with no fontset */
    while (i && !p->font_set) {
	i--;
	p++;
    }

    for (; --i >= 0; p++) {
	if (p->font_set)
	    fs = p->font_set;
	x += p->delta;
	esc = (*fs->methods->wc_draw_string) (dpy, d, fs, gc, x, y,
					      p->chars, p->nchars);
	if (!esc)
	    esc = fs->methods->wc_escapement (fs, p->chars, p->nchars);
	x += esc;
    }
}

void
XwcDrawString(dpy, d, font_set, gc, x, y, text, text_len)
    register Display   *dpy;
    Drawable            d;
    XFontSet            font_set;
    GC                  gc;
    int                 x, y;
    wchar_t            *text;
    int                 text_len;
{
    (void)(*font_set->methods->wc_draw_string) (dpy, d, font_set, gc, x, y,
						text, text_len);
}

void
XwcDrawImageString(dpy, d, font_set, gc, x, y, text, text_len)
    register Display   *dpy;
    Drawable            d;
    XFontSet            font_set;
    GC                  gc;
    int                 x, y;
    wchar_t            *text;
    int                 text_len;
{
    (*font_set->methods->wc_draw_image_string) (dpy, d, font_set, gc, x, y,
						text, text_len);
}

int 
XwcTextEscapement(font_set, text, text_len)
    XFontSet        font_set;
    wchar_t        *text;
    int             text_len;
{
    return (*font_set->methods->wc_escapement) (font_set, text, text_len);
}

int
XwcTextExtents(font_set, text, text_len,
	       overall_ink_extents, overall_logical_extents)
    XFontSet        font_set;
    wchar_t        *text;
    int             text_len;
    XRectangle      *overall_ink_extents;
    XRectangle      *overall_logical_extents;
{
    return (*font_set->methods->wc_extents) (font_set, text, text_len,
					     overall_ink_extents,
					     overall_logical_extents);
}

Status
XwcTextPerCharExtents(font_set, text, text_len,
		      ink_extents_buffer, logical_extents_buffer,
		      buffer_size, num_chars,
		      max_ink_extents, max_logical_extents)
    XFontSet        font_set;
    wchar_t        *text;
    int             text_len;
    XRectangle     *ink_extents_buffer;
    XRectangle     *logical_extents_buffer;
    int             buffer_size;
    int            *num_chars;
    XRectangle     *max_ink_extents;
    XRectangle     *max_logical_extents;
{
    return (*font_set->methods->wc_extents_per_char) 
	      (font_set, text, text_len, 
	       ink_extents_buffer, logical_extents_buffer,
	       buffer_size, num_chars, max_ink_extents, max_logical_extents);
}
