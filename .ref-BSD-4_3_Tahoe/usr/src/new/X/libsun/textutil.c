/*
 * textutil.c 
 *
 * Copyright (c) 1985 Massachusetts Institue of Technology
 * Copyright (c) 1986 Sun Microsystems, Inc.
 * Copyright (c) 1986 David C. Martin, UC Berkeley
 *
 * David C. Martin
 * ARPA: dcmartin@ingres.Berkeley.EDU
 * UUCP: ..!ucbvax!dcmartin
 *
 * $Log:	textutil.c,v $
 * Revision 10.3  86/11/29  13:49:03  jg
 * fixes from Berkeley
 * 
 * Revision 1.7  86/07/27  13:47:46  dcmartin
 * modifications to TextWidth() and CharWidth() to check for invalid characters
 * when determining width.
 * 
 * Revision 1.6  86/07/20  13:28:45  dcmartin
 * *** empty log message ***
 * 
 * Revision 1.5  86/07/17  10:38:19  dcmartin
 * release version w/ fix for correctly determining character widths in
 * variable width fonts
 * 
 * Revision 1.4  86/07/17  10:32:28  dcmartin
 *
 */

#ifndef lint
static char rcs_id[] = "$Header: textutil.c,v 10.3 86/11/29 13:49:03 jg Rel $";
#endif lint

#include <X/mit-copyright.h>

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

#ifdef sun

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

#define	CHUNK	80

extern int
CopyText (string, strlen, font, bm)
char	*string;
int	strlen;
FONT	*font;
BITMAP	*bm;
{
	int				i;
	struct pixrect			*region;
	register struct pixfont		*pf = (struct pixfont *) font->data;
	char				*buf = (char *) Xalloc(strlen + 1);

	/* XXX - to guarantee zero-termination (BARF!) */
	strncpy(buf, string, strlen);
	/* XXX - slow!! */
	region = mem_point(bm->width, bm->height, 1, (short *) bm->data);
	for (i = 0; i < strlen; i += CHUNK) {
		register int		j;
		struct pr_prpos		bat[CHUNK];

		for (j = 0; j < CHUNK && i + j < strlen; j++) {
			int				c = string[i + j];
			register struct pixchar		*pc = &(pf->pf_char[c]);

			bat[j].pr = pc->pc_pr;
			bat[j].pos = pc->pc_adv;
		}
	pr_batchrop(region, 0 - bat[0].pos.x, 0 - bat[0].pos.y, 
		PIX_SRC, bat, j);
	}
	/* XXX - slow */
	pr_destroy(region);
	free((caddr_t) buf);
} /* end CopyText() */

#undef CHUNK

/* 
 * Returns the width of a string in a font 
 */
extern int 
TextWidth (string, strlen, spacepad, font)
register char	*string;
register int	strlen;
int		spacepad;
register FONT	*font;
{
	register unsigned int c;
	register int width = 0;

	if (font->fixed) {
		width = strlen * font->avg_width;
		if (spacepad) {
			while (--strlen >= 0) {
				if (*string++ == font->space)
					width += spacepad;
			}
		}
	} else {
		register struct pixfont		*pf;

		pf = (struct pixfont *) font->data;
		while (--strlen >= 0) {
			c = *string++;
			if (c < font->first || c > font->last)
				continue;
			if (c == font->space)
				width += spacepad;
			if (pf->pf_char[c].pc_pr != (struct pixrect *) NULL)
				width += pf->pf_char[c].pc_adv.x;
		}
	}
	return (width);
} /* end TextWidth() */

/* 
 * Returns width of a character in a font. 
 */
extern int
CharWidth(c, font)
register unsigned int	c;
register FONT		*font;
{
	register struct pixfont	*pfp = (struct pixfont *) font->data;

	if (c < font->first || c > font->last)
		return (0);
	else if (font->fixed)
		return (font->avg_width);
	if (pfp->pf_char[c].pc_pr != (struct pixrect *) NULL)
		return(pfp->pf_char[c].pc_adv.x);
	else
		return(0);
} /* end CharWidth() */

#endif sun
