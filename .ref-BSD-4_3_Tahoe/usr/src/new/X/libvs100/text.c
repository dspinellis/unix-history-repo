/* $Header: text.c,v 10.3 86/02/01 15:47:40 tony Rel $ */
/* text.c	Prints a line of text
 *
 *	PrintText	Prints text with source font
 *	PrintTextMask	Prints text with mask font
 *	CopyText	Copy text to bitmap
 *	TextWidth	Returns width of a piece of text in a font
 *	CharWidth	Returns width of a character in a font
 *
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "vs100.h"

extern BitMap screen;
extern int VSReloc;
extern char FBMap[];
extern char SSMap[];

char *AllocateCopy(), *AllocateSpace();

PrintText (string, strlen, font, fore, back, charpad, spacepad, dstx, dsty,
	   clips, clipcount, func, zmask)
	char *string;
	FONT *font;
	int strlen, fore, back, charpad, spacepad, dstx, dsty;
	CLIP *clips;
	int clipcount, zmask;
	register int func;
{
	register PrintTextPacket *ptp;
#define	h ((PacketHeader *) ptp->ptp_head)
#define	dest ((BitMap *) ptp->ptp_destImage)
#define	destOff ((Point *) ptp->ptp_initialOffset.literal)
#define	txt ((TextString *) ptp->ptp_text)
#define	control ((ControlString *) ptp->ptp_control)
#define	clip ((RectangleList *) ptp->ptp_clipping.rectList)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	if ((string = (caddr_t) AllocateCopy(string, strlen)) == NULL ||
	    (ptp = (PrintTextPacket *) AllocateSpace (sizeof (PrintTextPacket))) == NULL)
	    return;

	if (fore & 1)
	    func += 0x20;
	if (back & 1)
	    func += 0x10;
	func = FBMap[func];
	h->ph_textMod.m_source = 1;	/* Source font */
	h->ph_textMod.m_mask = 0;	/* No mask font */
	h->ph_textMod.m_dest = 0;	/* Dest off. literal */
	h->ph_textMod.m_map = MAPTYPE(func);
	h->ph_textMod.m_textSize = 0;	/* 8 bit characters */
	h->ph_textMod.m_control = 0;	/* No control string */
	h->ph_opcode = PRINT_TEXT;
	*(long *) h->ph_next = NULL;
	*(caddr_t *) ptp->ptp_source.font = FDATA(font)->remote->vsPtr;

	*dest = screen;
	destOff->p_x = dstx;
	destOff->p_y = dsty;

	*(short *) ptp->ptp_map.literal = MAPLIT(func);

	if (clipcount == 1) {
	    h->ph_textMod.m_clipping = 1;
	    *(CLIP *) ptp->ptp_clipping.litRect = *clips;
	} else {
	    h->ph_textMod.m_clipping = 2;
	    *(caddr_t *) clip->r_first = (caddr_t) clips + VSReloc;
	    clip->r_count = clipcount;
	}

	*(caddr_t *) txt->t_first = (caddr_t) string + VSReloc;
	txt->t_count = strlen;

	*(short **) control->c_first = NULL;
	control->c_count = 0;
	ptp->ptp_charPad = charpad;
	ptp->ptp_spacePad = spacepad;

	WritePacket ((caddr_t) ptp);
#undef h
#undef dest
#undef destOff
#undef txt
#undef control
#undef clip
}

PrintTextMask (string, strlen, font, srcpix, charpad, spacepad, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *string;
	FONT *font;
	int strlen, srcpix, charpad, spacepad, dstx, dsty;
	CLIP *clips;
	int clipcount, zmask;
	register int func;
{
	register PrintTextPacket *ptp;
#define	h ((PacketHeader *) ptp->ptp_head)
#define	dest ((BitMap *) ptp->ptp_destImage)
#define	destOff ((Point *) ptp->ptp_initialOffset.literal)
#define	txt ((TextString *) ptp->ptp_text)
#define	control ((ControlString *) ptp->ptp_control)
#define	clip ((RectangleList *) ptp->ptp_clipping.rectList)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	if ((string = (caddr_t) AllocateCopy(string, strlen)) == NULL ||
	    (ptp = (PrintTextPacket *) AllocateSpace (sizeof (PrintTextPacket))) == NULL)
	    return;

	func = SSMap[func];
	h->ph_textMod.m_source = 0;	/* Constant */
	h->ph_textMod.m_mask = 1;	/* Mask font */
	h->ph_textMod.m_dest = 0;	/* Dest off. literal */
	h->ph_textMod.m_map = MAPTYPE(func);
	h->ph_textMod.m_textSize = 0;	/* 8 bit characters */
	h->ph_textMod.m_control = 0;	/* No control string */
	h->ph_opcode = PRINT_TEXT;
	*(long *) h->ph_next = NULL;
	ptp->ptp_source.const = srcpix & 1;

	*(caddr_t *) ptp->ptp_mask = FDATA(font)->remote->vsPtr;

	*dest = screen;
	destOff->p_x = dstx;
	destOff->p_y = dsty;

	*(short *) ptp->ptp_map.literal = MAPLIT(func);

	if (clipcount == 1) {
	    h->ph_textMod.m_clipping = 1;
	    *(CLIP *) ptp->ptp_clipping.litRect = *clips;
	} else {
	    h->ph_textMod.m_clipping = 2;
	    *(caddr_t *) clip->r_first = (caddr_t) clips + VSReloc;
	    clip->r_count = clipcount;
	}

	*(caddr_t *) txt->t_first = (caddr_t) string + VSReloc;
	txt->t_count = strlen;

	*(short **) control->c_first = NULL;
	control->c_count = 0;
	ptp->ptp_charPad = charpad;
	ptp->ptp_spacePad = spacepad;

	WritePacket ((caddr_t) ptp);
#undef h
#undef dest
#undef destOff
#undef txt
#undef control
#undef clip
}

CopyText (string, strlen, font, bm)
	char *string;
	int strlen;
	FONT *font;
	BITMAP *bm;
{
	register PrintTextPacket *ptp;
#define	h ((PacketHeader *) ptp->ptp_head)
#define	dst ((SubBitmap *) ptp->ptp_destImage)
#define	txt ((TextString *) ptp->ptp_text)
#define	control ((ControlString *) ptp->ptp_control)

	if ((string = (caddr_t) AllocateCopy(string, strlen)) == NULL ||
	    (ptp = (PrintTextPacket *) AllocateSpace (sizeof (PrintTextPacket))) == NULL)
	    return;

	h->ph_textMod.m_source = 1;	/* Source font */
	h->ph_textMod.m_mask = 0;	/* No mask font */
	h->ph_textMod.m_dest = 0;	/* Dest off. literal */
	h->ph_textMod.m_map = 0;
	h->ph_textMod.m_textSize = 0;	/* 8 bit characters */
	h->ph_textMod.m_control = 0;	/* No control string */
	h->ph_opcode = PRINT_TEXT;
	*(long *) h->ph_next = NULL;
	*(caddr_t *) ptp->ptp_source.font = FDATA(font)->remote->vsPtr;

	*(caddr_t *)dst->sb_address = BDATA(bm)->vsPtr;
	dst->sb_height = bm->height;
	dst->sb_width = bm->width;
	dst->sb_bitsPerPixel = 1;
	dst->sb_x = dst->sb_y = 0;

	*(short *) ptp->ptp_map.literal = 0;

	h->ph_textMod.m_clipping = 0;

	*(caddr_t *) txt->t_first = (caddr_t) string + VSReloc;
	txt->t_count = strlen;

	*(short **) control->c_first = NULL;
	control->c_count = 0;
	ptp->ptp_charPad = 0;
	ptp->ptp_spacePad = 0;

	WritePacket ((caddr_t) ptp);
#undef h
#undef dst
#undef txt
#undef control
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
		    width += widths[c - font->first];
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
	    return (FDATA(font)->widths[c - font->first]);
}
