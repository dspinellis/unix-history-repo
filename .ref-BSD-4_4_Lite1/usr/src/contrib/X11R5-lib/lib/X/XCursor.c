/* $XConsortium: XCursor.c,v 11.14 91/01/06 11:45:02 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1987	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include "Xlibint.h"
static XColor foreground = { 0,    0,     0,     0  };  /* black */
static XColor background = { 0, 65535, 65535, 65535 };  /* white */

Cursor XCreateFontCursor(dpy, which)
	Display *dpy;
	unsigned int which;
{
	/* 
	 * the cursor font contains the shape glyph followed by the mask
	 * glyph; so character position 0 contains a shape, 1 the mask for 0,
	 * 2 a shape, etc.  <X11/cursorfont.h> contains hash define names
	 * for all of these.
	 */

	if (dpy->cursor_font == None) {
	    dpy->cursor_font = XLoadFont (dpy, CURSORFONT);
	    if (dpy->cursor_font == None) return None;
	}

	return XCreateGlyphCursor (dpy, dpy->cursor_font, dpy->cursor_font, 
				   which, which + 1, &foreground, &background);
}

