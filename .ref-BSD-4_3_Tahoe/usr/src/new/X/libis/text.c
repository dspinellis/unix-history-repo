/*
 *	$Source: /u1/X/libis/RCS/text.c,v $
 *	$Header: text.c,v 1.1 86/11/17 14:34:33 swick Rel $
 */

#ifndef lint
static char *rcsid_text_c = "$Header: text.c,v 1.1 86/11/17 14:34:33 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	text.c
 *
 *	PrintText		Prints text with source font
 *	PrintTextMask		Prints text with mask font
 *	CopyText		Copy text to bitmap
 *	TextWidth		Returns width of a piece of text in a font
 *	CharWidth		Returns width of a character in a font
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

extern PIXMAP *MakePixmap();
extern PIXMAP *MakeFontPixmap();

/*
 *	PrintText			Prints text with source font
 */
PrintText(string, strlen, font, fore, back, charpad, spacepad, dstx, dsty, clips, clipcount, func, zmask)
char		*string;	/* string to print */
int		strlen;		/* length of string */
register FONT	*font;		/* source font */
int		fore;		/* foreground source pixel */
int		back;		/* background source pixel */
register int	charpad;	/* inter-character pad */
int		spacepad;	/* space-character pad */
int		dstx, dsty;	/* destination */
register CLIP	*clips;		/* clipping rectangles */
register int	clipcount;	/* count of clipping rectangles */
int		func;		/* GX display function */
int		zmask;		/* plane mask */
{
    register FontPriv *fdata = FDATA(font);
    PIXMAP *font_pixmap;

    CLIP bounds, i;

    bounds.top = dsty;
    bounds.height = font->height;

#ifdef DEBUG
if (debug & D_Text)
    printf("PrintText(\"%.*s\")\n", strlen, string);
#endif DEBUG

    font_pixmap = MakeFontPixmap(font, fore, back);

    for ( ; clipcount > 0; clipcount--, ++clips) {
	register int len = strlen;
	register char *p = string;
	register int x = dstx;

	while (len--) {
	    register int char_x;

	    /* complete clip rectangle for character */
	    bounds.left = x;
	    if (font->fixed) {
		char_x = font->avg_width * (*p - font->first);
		bounds.width = font->avg_width;
	    } else {
		char_x = fdata->xpos[*p - font->first];
		bounds.width = fdata->widths[*p - font->first];
	    }

	    /* advance x position for next character */
	    x += bounds.width + charpad;
	    if (*p == font->space) {
		x+= spacepad;
	    }

	    /* If clip rectangle and destination bounds overlap, display
	     * character in the area the two intersect */
	    if (Overlap(clips[0], bounds)) {
		i = Intersection(clips[0], bounds);
		CheckCursor(i);
		GIP_RasterOp((unsigned char)func,
		    font_pixmap, char_x + (i.left - bounds.left),
			    i.top-bounds.top,
		    &ScreenPixmap, i.left, i.top,
		    (BITMAP *)NULL, 0, 0,
		    i.width, i.height,
		    zmask);
	    }
	    ++p;
	}
    }
    RestoreCursor();
}

/*
 *	PrintTextMask
 */

PrintTextMask(string, strlen, font, srcpix, charpad, spacepad, dstx, dsty, clips, clipcount, func, zmask)
char		*string;	/* string to print */
int		strlen;		/* length of string */
register FONT	*font;		/* source font */
int		srcpix;		/* source pixel */
register int	charpad;	/* inter-character pad */
int		spacepad;	/* space-character pad */
int		dstx, dsty;	/* destination */
register CLIP	*clips;		/* clipping rectangles */
register int	clipcount;	/* count of clipping rectangles */
int		func;		/* GX display function */
int		zmask;		/* plane mask */
{
    register FontPriv *fdata = FDATA(font);
    CLIP bounds, i;
    PIXMAP	*textpix;

#ifdef DEBUG
if (debug & D_Text)
    printf("PrintTextMask(string=\"%.*s\", srcpix=0x%x, func=%d, zmask=0x%04x)\n",
	strlen, string, srcpix, func, zmask);
#endif DEBUG

    textpix = (PIXMAP *) MakePixmap((BITMAP *)NULL, srcpix, 0);

    bounds.top = dsty;
    bounds.height = font->height;

    for ( ; clipcount > 0; clipcount--, ++clips) {
	register int len = strlen;
	register char *p = string;
	register int x = dstx;

	while (len--) {
	    register int char_x;

	    /* complete clip rectangle for character */
	    bounds.left = x;
	    if (font->fixed) {
		char_x = font->avg_width * (*p - font->first);
		bounds.width = font->avg_width;
	    } else {
		char_x = fdata->xpos[*p - font->first];
		bounds.width = fdata->widths[*p - font->first];
	    }

	    /* advance x position for next character */
	    x += bounds.width + charpad;
	    if (*p == font->space) {
		x+= spacepad;
	    }

	    /* If clip rectangle and destination bounds overlap, display
	     * character in the area the two intersect */
	    if (Overlap(clips[0], bounds)) {
		i = Intersection(clips[0], bounds);
		CheckCursor(i);
		GIP_RasterOp((unsigned char)func,
		    textpix, 0, 0,
		    &ScreenPixmap, i.left, i.top,
		    fdata->mask, char_x + (i.left - bounds.left),
			    i.top-bounds.top,
		    i.width, i.height,
		    zmask);
	    }
	    ++p;
	}
    }
    RestoreCursor();
    if (!--textpix->refcnt)
	FreePixmap(textpix);
}

/*
 *	CopyText
 */
CopyText(string, strlen, font, dst)
register char	*string;	/* string to copy */
register int	strlen;		/* length of string */
register FONT	*font;		/* source font */
BITMAP		*dst;		/* destination bitmap */
{
    register FontPriv *fdata = FDATA(font);
    PIXMAP *font_pixmap, *dst_pixmap;
    register int height = font->height;
    register x = 0;

#ifdef DEBUG
if (debug & D_Text)
    printf("CopyText(string=\"%.*s\")\n", strlen, string);
#endif DEBUG

    font_pixmap = MakePixmap(fdata->mask, 1, 0);
    dst_pixmap = MakePixmap(dst, 1, 0);

    while (strlen--) {
	register int width;

	/* get width of this character */
	if (font->fixed) {
	    width = font->avg_width;
	} else {
	    width = fdata->widths[*string - font->first];
	}

	GIP_RasterOp(GXcopy,
	    font_pixmap, fdata->xpos[*string - font->first], 0,
	    dst_pixmap, x, 0,
	    (BITMAP *)NULL, 0, 0,
	    width, height,
	    1);

	/* advance x position for next character */
	x += width;

	++string;
    }

    FreePixmap(font_pixmap);
    FreePixmap(dst_pixmap);
}

/*
 *	TextWidth	Returns the width of a string in a font
 */
int
TextWidth(string, strlen, spacepad, font)
char		*string;
register int	strlen;
register int	spacepad;
register FONT	*font;
{
    register u_char *strptr = (u_char *) string;
    register short c;
    register short *widths;
    register int width = 0;

    if (font->fixed) {
	width = strlen * font->avg_width;
	if (spacepad) {
	    while (strlen--) {
		if (*strptr++ == font->space)
		    width += spacepad;
	    }
	}

    } else {
	widths = FDATA(font)->widths;
	while (strlen--) {
	    c = *strptr++;
	    if (c >= font->first && c <= font->last) {
		if (c == font->space)
		    width += spacepad;
		width += widths[c - font->first];
	    }
	}
    }

    return (width);
}

/*
 *	CharWidth	Returns width of a character in a font.
 */
int
CharWidth(c, font)
unsigned int	c;
register FONT	*font;
{

    if (c < font->first || c > font->last)
	return (0);
    else if (font->fixed)
	return (font->avg_width);
    else
	return (FDATA(font)->widths[c - font->first]);
}
