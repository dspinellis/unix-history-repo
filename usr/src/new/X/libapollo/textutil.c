#ifndef lint
static char *rcsid_textutil_c = "$Header: textutil.c,v 10.1 86/11/29 13:53:04 jg Rel $";
#endif	lint
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

/* textutil.c	text related utility routines.
 *
 *	CopyText	Copy text to bitmap
 *	TextWidth	Returns width of a piece of text in a font
 *	CharWidth	Returns width of a character in a font
 *
 */

#include "Xapollo.h"

status_$t status;

CopyText (string, strlen, font, bm)
	char *string;
	int strlen;
	FONT *font;
	BITMAP *bm;
{
    FontPriv	* fp;

    fp = (FontPriv *)font->data;
    gpr_$set_text_font( (short)fp->ap_font_id, status);
    gpr_$set_bitmap( bm->data, status);
    gpr_$text(*string, (short)strlen, status);
    gpr_$set_bitmap(Screen.bm);
}

/* Returns the width of a string in a font
 * Works only for Apollo-type fonts
 */
int TextWidth (string, strlen, spacepad, font)
	register char *string;
	register int strlen;
	int spacepad;
	register FONT *font;
{
	register int width = 0;

	if (font->fixed) {
	    width = strlen * font->avg_width;
	    if (spacepad) {
		while (--strlen >= 0)
		    if (*string++ == font->space)
		        width += spacepad;
	      }
	  } else {
	    FontPriv	* fp;
	    gpr_$offset_t size;

	    fp = (FontPriv *)font->data;
	    gpr_$set_text_font( (short)fp->ap_font_id, status);
	    gpr_$inq_text_extent(*string, (short)strlen, size, status);
	    width = size.x_size;
	  }
	
	return (width);
}

/* Returns width of a character in a font.
 * Works only for Apollo-type fonts
 */
int 
CharWidth(c, font)
	register unsigned int c;
	register FONT *font;
{ 
    FontPriv	* fp;
    int width;

    if (c < font->first || c > font->last)
        return (0);
    fp = (FontPriv *)font->data;
    gpr_$inq_character_width( (short)fp->ap_font_id, c, (short)width, status);
    return(width);
}
