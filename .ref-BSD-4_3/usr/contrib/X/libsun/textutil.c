#ifndef lint
static char *rcsid_textutil_c = "$Header: textutil.c,v 10.2 86/02/01 16:21:27 tony Rel $";
#endif	lint
#ifdef	sun
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef	lint
static char sccsid[] = "@(#)textutil.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* textutil.c	text related utility routines.
 *
 *	CopyText	Copy text to bitmap
 *	TextWidth	Returns width of a piece of text in a font
 *	CharWidth	Returns width of a character in a font
 *
 */

/*
 *	ToDo:
 *		Use static pixrects
 */

#include "Xsun.h"

CopyText (string, strlen, font, bm)
	char *string;
	int strlen;
	FONT *font;
	BITMAP *bm;
{
    struct pixrect *region;
    int         i;
    register struct pixfont *pf = (struct pixfont *) font->data;
    char       *buf = (char *) Xalloc(strlen + 1);

    strncpy(buf, string, strlen);	/* XXX - to guarantee
					 * zero-termination (BARF!) */
    region = mem_point(bm->width, bm->height, 1, (short *) bm->data);	/* XXX - slow!! */
#define	CHUNK	80
    for (i = 0; i < strlen; i += CHUNK) {
	register int j;
	struct pr_prpos bat[CHUNK];

	for (j = 0; j < CHUNK && i + j < strlen; j++) {
	    int         c = string[i + j];
	    register struct pixchar *pc = &(pf->pf_char[c]);

	    bat[j].pr = pc->pc_pr;
	    bat[j].pos = pc->pc_adv;
	}
	pr_batchrop(region, 0 - bat[0].pos.x, 0 - bat[0].pos.y, PIX_SRC, bat, j);
    }
    pr_destroy(region);		/* XXX - slow */
    free((caddr_t) buf);
}

/* Returns the width of a string in a font */

int TextWidth (string, strlen, spacepad, font)
	register char *string;
	register int strlen;
	int spacepad;
	register FONT *font;
{
	register unsigned int c;
	register int width = 0;

	if (font->fixed) {
	    width = strlen * font->avg_width;
	    if (spacepad) {
		while (--strlen >= 0)
		    if (*string++ == font->space)
		        width += spacepad;
	    }
	} else {
	    register struct pixfont *pf = (struct pixfont *)font->data;
	    while (--strlen >= 0) {
		c = *string++;
		if (c >= font->first && c <= font->last) {
		    if (c == font->space)
		        width += spacepad;
		    width += pf->pf_char[c].pc_adv.x;
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
	    return (((struct pixfont *) font->data)->pf_char[c].pc_adv.x);
}
#endif	sun
