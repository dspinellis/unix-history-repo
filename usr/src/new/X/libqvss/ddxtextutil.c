/* Copyright 1985, Massachusetts Institute of Technology */

/* textutil.c	text related utility routines.
 *
 *	CopyText	Copy text to bitmap
 *	TextWidth	Returns width of a piece of text in a font
 *	CharWidth	Returns width of a character in a font
 *
 */

#include "ddxqvss.h"
#include "vstagbl.h"

CopyText (string, strlen, font, bm)
	register char *string;
	register int strlen;
	FONT *font;
	register BITMAP *bm;
{

	int dstx,srcx,w,h;
	FontPriv *pfont;
	BITMAP *fbm;

	pfont = FDATA(font);
	fbm = pfont->strike;

	dstx = 0;
	h = font->height;

	while (strlen--)
		{

		/* SET THE WIDTH OF THE CHARACTER */

		w = pfont->widths[*string];
		srcx = pfont->leftarray[*string++];

		/* CALL THE COPY BITMAP PROCEDURE TO COPY THE CHARACTER
		   FROM THE FONT BITMAP TO THE DESTINATION BITMAP */

		copyrmsk(VSTA$K_SRC_BITMAP, (short *)fbm->data, fbm->width,
			 fbm->height, srcx, 0, w, h, 
			 (short *)bm->data, bm->width, bm->height, dstx, 0,
			 VSTA$K_MAP_SRC, 0, 0);

		/* ADD THE WIDTH OF THE CHARACTER TO THE DST OFFSET */

		dstx += w;

		};
}

/* Returns the width of a string in a font */

int TextWidth (string, strlen, spacepad, font)
	char *string;
	register int strlen;
	int spacepad;
	register FONT *font;
{
	register u_char *strptr = (u_char *) string;
	short c;
	register short *widths;
	int width = 0;

	if (font->fixed) {
	    width = strlen * font->avg_width;
	    if (spacepad) {
		while (--strlen >= 0)
		    if (*strptr++ == font->space)
		        width += spacepad;
	    }
	} else {
	    widths = FDATA(font)->widths;
	    while (--strlen >= 0) {
		c = *strptr++;
		if (c >= font->first && c <= font->last) {
		    if (c == font->space)
		        width += spacepad;
		    width += widths[c];
		}
	    }
	}

	return (width);
}

/* Returns width of a character in a font. */

int CharWidth(c, font)
	register unsigned int c;
	register FONT *font;
{

	if (c < font->first || c > font->last)
	    return (0);
	else if (font->fixed)
	    return (font->avg_width);
	else
	    return (FDATA(font)->widths[c]);
}
