#ifndef lint
static char *rcsid_text_c = "$Header: text.c,v 10.1 86/11/19 10:44:22 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* text.c - X text based functions
 *
 *      PrintText       Prints text with source font
 *      PrintTextMask   Prints text with mask font
 *      CopyText        Copy text to bitmap
 *      TextWidth       Returns width of a piece of text in a font
 *      CharWidth       Returns width of a character in a font
 *	OffScreenText	Utility rtn to optimize text blts using 
 *			an offscreen buffer
 *	CopyScreenBits	Utility rtn used to grab a rectangular area
 *			of the screen and place it in an offscreen buffer.
 *
 *  	Authors:
 *		Dan Stone & Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"
#include "text.h"

/*
 * Print text with font as source 
 */

PrintText (string, strlen, font, fore, back, charpad, spacepad, dstx, dsty,
           clips, clipcount, func, zmask)
	register char *string;
	FONT *font;
	register strlen, dstx;
	register charpad, spacepad;
	int fore, back, dsty;
	CLIP *clips;
	int clipcount, zmask;
	int func;
{
	FontPriv *fpriv = FDATA(font);
	BITMAP *cbm = fpriv->chrs;
	register c;
	register charwidth;
	register buf_wd, buf_ht;
	register nchar;
	register srcOx;
	int srcCx;

#ifdef TRACE_X
	fprintf(stderr, "In PrintText\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Limit text to one plane
	 */
	if ((zmask & 1) == 0)
		return;

	/*
	 * Change combination rule to reflect specified
	 * foreground and background colors
	 */

	if (fore & 1)
		func += 0x20;

	if (back & 1)
		func += 0x10;

	func = FBMap[func];

	/*
	 * While we have more than CH_THRESHOLD characters then we will put
	 * the text into an offscreen buffer then blow it up onto the screen.
	 * When it drops below this threshold then it is no longer efficient
	 * to go to an offscreen buffer and we will go directly to the screen
	 * with bitblt.
	 */

	if (fpriv->offscr != NILBITMAP) {
		while (strlen > CH_THRESHOLD) {
			/*
			 * Make sure that CopyScreenBits() stays on the screen.
			 * Note: We are assuming the destination bitmap and the
			 *	 screen bitmap are already zero based and we
			 *       are assuming that the txtbm is at least the
			 *       width of the screen. 
			 */

			if ((buf_ht = font->height) > (pbm.height - dsty)) {
				buf_ht = pbm.height - dsty;
			}

			if ((buf_wd = ((fpriv->maxwidth + charpad + spacepad) *
			    strlen)) > (pbm.width - dstx)) {
				if ((buf_wd = pbm.width - dstx) > txtbm.width) {
					buf_wd = txtbm.width;
				}
			}

			/*
			 * Make sure the sizes are non-zero.
			 */

			if (buf_ht < 1 || buf_wd < 1) {
				break;
			}

			/*
			 * Bring area on the screen into the offscreen buffer.
			 */

			CopyScreenBits(dstx, dsty, buf_wd, buf_ht, &txtbm);

			/*
			 * Blt as much text as one can into the offscreen
			 * buffer. NOTE: Because CopyScreenBits rounds over
			 * to a long word (4 bytes) boundary we must move
			 * the text over in the offscreen area by modulo 32.
			 * Also note that offscreen_text returns the source
			 * corner in srcCx.
			 */

			srcOx = dstx & 0x1F;
			if ((nchar = OffScreenText(&txtbm,font,string,strlen,
						    charpad,spacepad,srcOx,
						    &srcCx,func)) < 1) {
				/*
				 * Check nchar before going on and get out of
				 * the loop if its less than 1.
				 */

				break;
			}

			/*
			 * Move the string up and the char count down.
			 */

			string += nchar;
			strlen -= nchar;

			/*
			 * Put the offscreen buffer back onto the screen using
			 * CopyBits().
			 *
			 * Make source and destination rectangles.
			 */

			FillInRect(srcOx,0,srcCx - srcOx,buf_ht,&SrcRect);
			FillInRect(dstx,dsty,(srcCx - srcOx),
				   buf_ht,&DstRect);

			/*
			 * Put the offscreen text buffer back onto the screen.
			 */

			CopyBits((u_short *)txtbm.data,txtbm.width,txtbm.height,
				 &SrcRect, (u_short *) pbm.data, pbm.width,
				 pbm.height, &DstRect, NILMASK, NIL, NIL,
				 GXcopy, clipcount, clips);

			/*
			 * Adjust destination address (dstx)
			 */

			dstx = DstRect.corner_x + charpad;
		}
	}

	/*
	 * Now loop thru the string until we are finished.....
	 */

	while (--strlen >= 0) {

		/*
		 * Character we are working on
		 */

		c = (int) (*string++);

		/*
		 * Check for legal character.
		 */

		if (c < font->first || c > font->last)
			continue;

		/*
		 * Get width of current character
		 */

		if((charwidth = fpriv->widths[c]) > 0) {

			/*
			 * Make source and destination rectangles
			 */

			FillInRect(0,font->height * (c - font->first),
				   charwidth, font->height, &SrcRect);
			FillInRect(dstx, dsty, charwidth, font->height,
				  &DstRect);

			/*
			 * Blt character to frame buffer
			 */

			CopyBits((u_short *) cbm->data, cbm->width, cbm->height,
				 &SrcRect, (u_short *) pbm.data, pbm.width,
				 pbm.height, &DstRect, NILMASK, NIL, NIL, func,
				 clipcount, clips);

			/*
			 * Adjust destination address (dstx)
			 */

			dstx += charwidth + charpad;
		}

		/*
		 * Adjust destination address by size of space character
		 */

		if (c == font->space)
			dstx += spacepad;
	}
}

/*
 * Print text with font as mask
 */

PrintTextMask (string, strlen, font, srcpix, charpad, spacepad, dstx, dsty,
               clips, clipcount, func, zmask)
        register char *string;
        FONT *font;
	register strlen, charpad, spacepad, dstx;
        int srcpix, dsty;
        CLIP *clips;
        int clipcount, zmask;
        int func;
{
        register FontPriv *fpriv = FDATA(font);
	int charsize = BitmapSize(fpriv->maxwidth, font->height);
	u_short *charmask;
	register charwidth;
	register c;
	register srcOx;
	register nchar;
	int srcCx;

#ifdef TRACE_X
        fprintf (stderr, "In PrintTextMask\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Limit text to one plane
	 */

        if ((zmask & 1) == 0)
            return;

	/*
	 * While we have more than CH_THRESHOLD characters then we will put
	 * the text into an offscreen buffer then blow it up onto the screen.
	 * When it drops below this threshold then it is no longer efficient
	 * to go to an offscreen buffer and we will go directly to the screen
	 * with bitblt.
	 */

	if (fpriv->offscr != NILBITMAP) {
		while (strlen > CH_THRESHOLD) {

			/*
			 * Clear the offscreen buffer area.
			 */

			bzero((char *)txtbm.data,
			      (font->height * ((txtbm.width + 15)/16)) << 1);

			/*
			 * Blt as much text as one can into the offscreen
			 * buffer.  Note that offscreen_text returns the source
			 * corner in srcCx.
			 */

			srcOx = dstx & 0xF;
			if ((nchar = OffScreenText(&txtbm,font,string,strlen,
						    charpad,spacepad,srcOx,
						    &srcCx,GXcopy)) < 1) {
				/*
				 * Check nchar before going on and get out of
				 * the loop if its less than 1.
				 */

				break;
			}

			/*
			 * Move the string up and the char count down.
			 */

			string += nchar;
			strlen -= nchar;

			/*
			 * Put the offscreen buffer back onto the screen using
			 * CopyBits().
			 *
			 * Make source and destination rectangles.
			 */

			FillInRect((dstx - srcOx),dsty,srcCx,
				   font->height,&DstRect);

			/*
			 * Using the offscreen bitmap as a mask, copy
			 * the text to the screen.
			 */

			CopyBits((u_short *) ConstantTiles[srcpix & 1],
				 NIL, NIL, NILRECT,
				 (u_short *) pbm.data, pbm.width, pbm.height,
				 &DstRect, (u_short *)txtbm.data, txtbm.width,
				 font->height, MAKE_TILE_RULE(func), clipcount,
				 clips);

			/*
			 * Adjust destination address (dstx)
			 */

			dstx = DstRect.corner_x + charwidth + charpad;
		}
	}

	/*
	 * Now loop thru the string until we are finished.....
	 */

	while (--strlen >= 0) {

		/*
		 * Character we are working on
		 */

		c = (int) (*string++);

		/*
		 * Check for legal character.
		 */

		if (c < font->first || c > font->last)
			continue;

		/*
		 * Get width of current character
		 */

		if((charwidth = fpriv->widths[c]) > 0) {

			/*
			 * Use character as clipping mask
			 */

                        charmask = (u_short *) ((long) fpriv->chrs->data +
                                   charsize * (c - font->first));

			/*
			 * Fill in destination rectangle
			 */

			FillInRect(dstx, dsty, charwidth, font->height,
				  &DstRect);

			/*
			 * Blt character to frame buffer using font
			 * character as mask
			 */

			CopyBits((u_short *) ConstantTiles[srcpix & 1], NIL,
				 NIL, NILRECT, (u_short *) pbm.data, pbm.width,
				 pbm.height, &DstRect,
				 charmask, charwidth, font->height,
				 MAKE_TILE_RULE(func), clipcount, clips);

			/*
			 * Adjust destination address (dstx) by character
			 * pad amount
			 */

			dstx += charwidth + charpad;
		}

		/*
		 * Adjust destination address by size of space character
		 */

		if (c == font->space)
			dstx += spacepad;
	}
}

/*
 * Copy text to bitmap
 */

CopyText (string, strlen, font, bm)
        register char *string;
        register strlen;
        FONT *font;
        BITMAP *bm;
{
	register FontPriv *fpriv = FDATA(font);
	BITMAP *cbm = fpriv->chrs;
	register c;
	register charwidth;
	register dstx = 0;

#ifdef TRACE_X
	fprintf(stderr, "In CopyText\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * loop thru string until we are finished.....
	 */

	while (--strlen >= 0) {

		/*
		 * Character we are working on
		 */

		c = (int) (*string++);

		/*
		 * Check for legal character.
		 */

		if (c < font->first || c > font->last)
			continue;

		/*
		 * Get width of current character
		 */

		if((charwidth = fpriv->widths[c]) == 0)
			continue;

		/*
		 * Fill in source and destination rectangles
		 */

		FillInRect(0,font->height * (c - font->first),
			   charwidth, font->height, &SrcRect);
		FillInRect(dstx, 0, charwidth, font->height, &DstRect);

		/*
		 * Blt character to bitmap
		 */

		CopyBits((u_short *)cbm->data,cbm->width, cbm->height, &SrcRect,
			 (u_short *) bm->data, bm->width, bm->height, &DstRect,
			 NILMASK, NIL, NIL, GXcopy, 0, NILCLIP);

		/*
		 * Adjust destination address (dstx)
		 */

		dstx += charwidth;
	}
}

/*
 * Compute width of a piece of text in a font
 */

int TextWidth (string, strlen, spacepad, font)
        register char *string;
        register int strlen;
        int spacepad;
        register FONT *font;
{
        register u_int c;
        register short *widths;
        register int width = 0;

#ifdef TRACE_X
	fprintf(stderr, "In TextWidth\n");
	fflush(stderr);
#endif TRACE_X

        if (font->fixed) {
		/*
		 * Font is fixed width
		 */

		/*
		 * Compute total width of text string
		 */

		width = strlen * font->avg_width;

		if (spacepad) {
			/*
			 * Add spacepad amount to the total width for each
			 * space character found in text string
			 */

			while (--strlen >= 0) {
				if (*string++ == font->space)
					width += spacepad;
			}
		}
        } else	{
		/*
		 * Font is variable width
		 */


		/*
		 * get pointer to width table
		 */

		widths = FDATA(font)->widths;

		/*
		 * Loop thru text string
		 */

		while (--strlen >= 0) {
			/*
			 * Get current character
			 */

			c = *string++;
			if (c >= font->first && c <= font->last) {
				/*
				 * Valid character
				 */

				if (c == font->space) {
					/*
					 * Character is a space so add
					 * spacepad amount to total width
					 */

					width += spacepad;
				}

				/*
				 * Find width of this character in width
				 * table and add it to total
				 */

				width += widths[c];
			}
		}
        }
        return (width);
}

/*
 * Determine width of a character in a font
 */

int CharWidth(c, font)
        register u_int c;
        register FONT *font;
{
#ifdef TRACE_X
	fprintf(stderr, "In CharWidth\n");
	fflush(stderr);
#endif TRACE_X

	if (c < font->first || c > font->last)
		/*
		 * Character not in font 
		 */

		return (0);
	else if (font->fixed)
		/*
		 * Font is a fixed width so return the
		 * average character width for the font
		 */

		return (font->avg_width);
	else
		/*
		 * Font is variable width so determine
		 * width  of character from font width table
		 */

		return (FDATA(font)->widths[c]);
}

/*
 * OffScreenText puts text from "font" into "buf_bm".  It assumes the font
 * bitmap * has its characters "left justified".  It returns the rightmost
 * X position in "cornerX".
 */

static
OffScreenText(buf_bm,font,string,ch_count,ch_pad,sp_pad,dstX,cornerX,func)
	BITMAP	*buf_bm;	/* Offscreen buffer bitmap */
	FONT	*font;		/* Font structure */
	char	*string;	/* String of characters to be put up. */
	int	ch_count;	/* Number of characters in the string */
	int	ch_pad;		/* Space between characters */
	int	sp_pad;		/* Amount of space a pad character is. */
	int	dstX;		/* The spot in the offscreen buffer where
			   	   the text is to start. */
	int	*cornerX;	/* Send the rightmost X position back to the
				   caller for use with the bltter. */
	int	func;		/* Function (or combination rule) to be used
				   with text */
{
	register u_char *dst;
	register u_char *src;
	register u_long src_nextline, dst_nextline;
	register int height;
	register u_short leftmask, rtmask;
	int c, shift, ndst_shorts, inv_shift, i, start_dst;
	int dst_nextcol, src_nextcol, nsrc_shorts, ht_plus1;
	short chrs_nshorts, buf_nshorts;
	u_long bitptr;
	long maxbitptr;
	int shorts_per_char;
	u_char *chrs_data;
	FontPriv *fpriv =  FDATA(font);
	short *char_widths = fpriv->widths;

#ifdef TRACE_X
        fprintf (stderr, "In OffScreenText\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Calculate, in shorts, the width of both the chrs and
	 * buffer bitmaps.
	 */

	chrs_nshorts = BTOW(fpriv->maxwidth);

	maxbitptr = fpriv->offscr->width;
	buf_nshorts = BTOW(maxbitptr);

	/*
	 * Set up to go through the string of characters.
	 */

	bitptr = dstX;

	src_nextline = MUL_2(chrs_nshorts);
	dst_nextline = MUL_2(buf_nshorts);

	/*
	 * Setup for text blt
	 */

	ht_plus1 = font->height + 1;
	shorts_per_char = MUL_2(chrs_nshorts * font->height);
	chrs_data = (u_char *)fpriv->chrs->data;

	/*
	 * Blt text string into offscreen buffer
	 */

	switch (func) {
		case (GXclear):
		     CopyText_LOOP(GXclear_MASK, GXclear_OP);
		     break;

		case (GXand):
		     CopyText_LOOP(GXand_MASK, GXand_OP);
		     break;

		case (GXandReverse):
		     CopyText_LOOP(GXandReverse_MASK, GXandReverse_OP);
		     break;

		case (GXcopy):
		     CopyText_LOOP(GXcopy_MASK, GXcopy_OP);
		     break;

		case (GXandInverted):   
		     CopyText_LOOP(GXandInverted_MASK, GXandInverted_OP);
		     break;

		case (GXnoop):
		     break;

		case (GXxor):
		     CopyText_LOOP(GXxor_MASK, GXxor_OP);
		     break;

		case (GXor):
		     CopyText_LOOP(GXor_MASK, GXor_OP);
		     break;

		case (GXnor):
		     CopyText_LOOP(GXnor_MASK, GXnor_OP);
		     break;

		case (GXequiv):
		     CopyText_LOOP(GXequiv_MASK, GXequiv_OP);
		     break;

		case (GXinvert):
		     CopyText_LOOP(GXinvert_MASK, GXinvert_OP);
		     break;

		case (GXorReverse):
		     CopyText_LOOP(GXorReverse_MASK, GXorReverse_OP);
		     break;

		case (GXcopyInverted):
		     CopyText_LOOP(GXcopyInverted_MASK, GXcopyInverted_OP);
		     break;

		case (GXorInverted):
		     CopyText_LOOP(GXorInverted_MASK, GXorInverted_OP);
		     break;

		case (GXnand):
		     CopyText_LOOP(GXnand_MASK, GXnand_OP);
		     break;

		case (GXset):
		     CopyText_LOOP(GXset_MASK, GXset_OP);
		     break;
	}

	/*
	 * Remove the last charcter pad added to the last character.
	 */

	bitptr -= ch_pad;

	/*
	 * Return right most X value and character count to caller
	 */

	if (bitptr > dstX)
		*cornerX = bitptr;
	else
		*cornerX = dstX;
	return(ch_count);
}

#if (APA8 || APA8C)

#define DSTBITS	((u_short *)DstBits)
#define SRCBITS	((u_short *)SrcBits)
#define SIZE_OF_SHORT (sizeof(u_short))
#define SIZE_OF_APA8_SHORT (sizeof(u_short) * 2)

/*
 * Copy shorts off of the screen (pbm) into the destination bitmap.
 */

static
CopyScreenBits(StartX, StartY, Width, Height, DstBitmap)
	int StartX, StartY;
	int Width;
	register Height;
	BITMAP *DstBitmap;
{
	register nwords;
	register char *DstBits;
	register DstRowBytes;
	register SrcRowBytes;
	register char *SrcBits;

#ifdef TRACE_X
        fprintf (stderr, "In CopyScreenBits\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Check StartX and StartY and offset the destination pointer if
	 * one or both are negative and change the source pointer.
	 */

	if (StartX < 0) {
		/*
		 * Make sure there are words to be taken off the screen.
		 */

		if ((nwords = BTOW(StartX + Width)) < 1) {
			return;
		}
		DstRowBytes = MUL_2(DIV_BPW(DstBitmap->width) - nwords);
		SrcRowBytes = MUL_4(DIV_BPW(pbm.width));
		DstBits = (char *)DstBitmap->data + MUL_2(DIV_BPW(-StartX));

		if (StartY < 0) {
			/*
			 * Figure out the number of scanlines to be copied.
			 */

			if ((Height += StartY) < 1) {
				return;
			}

			DstBits += (-StartY * (DstBitmap->width >> 3));
			SrcBits = (char *)pbm.data;
		} else {
			SrcBits = (char *)pbm.data + (StartY * SrcRowBytes);
		}
	} else {
		/*
		 * Make sure there are words to be taken off the screen.
		 */

		if ((nwords = BTOW(StartX + Width) - DIV_BPW(StartX)) < 1) {
			return;
		}

		DstRowBytes = MUL_2(DIV_BPW(DstBitmap->width) - nwords);
		SrcRowBytes = MUL_4(DIV_BPW(pbm.width));
		DstBits = (char *)DstBitmap->data;

		if (StartY < 0) {
			/*
			 * Figure out the number of scanlines to be copied.
			 */

			if ((Height += StartY) < 1) {
				return;
			}

			DstBits += (-StartY * (DstBitmap->width >> 3));
			SrcBits = (char *)pbm.data + MUL_4(DIV_BPW(StartX));
		} else {
			SrcBits = (char *)pbm.data + (StartY * SrcRowBytes) +
				  MUL_4(BTOW(StartX));
		}
	}


	if (nwords > 1) {	/* more than one short wide */
		int tmpnWords = nwords;

		SrcRowBytes -= MUL_4(nwords);
		while(1) {
			do {
				*DSTBITS = *SRCBITS;
				DstBits += SIZE_OF_SHORT;
				SrcBits += SIZE_OF_APA8_SHORT;
			} while(--nwords > 0);

			if(--Height) {
				DstBits += DstRowBytes;
				SrcBits += SrcRowBytes;
				nwords = tmpnWords;
			} else {
				break;
			}
	    	}
	} else {		/* one short wide */
		/*
		 * Should not have subtracted (nwords << 2) from
		 * DstRowBytes.  So now that we know nwords == 1 we will
		 * add (nwords << 2) == 4 back to DstRowBytes.
		 */

		DstRowBytes += 4;

	    	while(1) {
			*DSTBITS = *SRCBITS;
			if(--Height) {
				DstBits += DstRowBytes;
				SrcBits += SrcRowBytes;
			} else {
				break;
			}
	    	}
	}
}

#else

#define DSTBITS	((u_long *)DstBits)
#define SRCBITS	((u_long *)SrcBits)
#define SIZE_OF_LONG sizeof(u_long)

/*
 * Copy long words off of the screen (pbm) into the destination bitmap.
 */

static
CopyScreenBits(StartX, StartY, Width, Height, DstBitmap)
	int StartX, StartY;
	int Width;
	register Height;
	BITMAP *DstBitmap;
{
	register nwords;
	register char *DstBits;
	register DstRowBytes;
	register SrcRowBytes;
	register char *SrcBits;
#if (APA16 && USE_APA16_HDWR)
	int tmp;
#endif (APA16 && USE_APA16_HDWR)

#ifdef TRACE_X
        fprintf (stderr, "In CopyScreenBits\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Check StartX and StartY and offset the destination pointer if
	 * one or both are negative and change the source pointer.
	 */

	if (StartX < 0) {
		/*
		 * Make sure there are words to be taken off the screen.
		 */

		if ((nwords = BTOL(StartX + Width)) < 1) {
			return;
		}

		/*
		 * DstRowBytes and SrcRowBytes are MUL_4'ed so that
		 * they are the number of BYTES the address must be incremented.
		 */

		DstRowBytes = MUL_4(DIV_BPL(DstBitmap->width) - nwords);
		SrcRowBytes = MUL_4(DIV_BPL(pbm.width));
		DstBits = (char *)DstBitmap->data + MUL_4(DIV_BPL(-StartX));

		if (StartY < 0) {
			/*
			 * Figure out the number of scanlines to be copied.
			 */

			if ((Height += StartY) < 1) {
				return;
			}

			DstBits += (-StartY * (DstBitmap->width >> 3));
			SrcBits = (char *)pbm.data;
		} else {
			SrcBits = (char *)pbm.data + (StartY*SrcRowBytes);
		}
	} else {
		/*
		 * Make sure there are words to be taken off the screen.
		 */

		if ((nwords = BTOL(StartX + Width) - DIV_BPL(StartX)) < 1) {
			return;
		}

		DstRowBytes = MUL_4(DIV_BPL(DstBitmap->width) - nwords);
		SrcRowBytes = MUL_4(DIV_BPL(pbm.width));
		DstBits = (char *)DstBitmap->data;

		if (StartY < 0) {
			/*
			 * Figure out the number of scanlines to be copied.
			 */

			if ((Height += StartY) < 1) {
				return;
			}

			DstBits += (-StartY * (DstBitmap->width >> 3));
			SrcBits = (char *)pbm.data + MUL_4(DIV_BPL(StartX));
		} else {
			SrcBits = (char *)pbm.data + (StartY * SrcRowBytes) +
				  MUL_4(DIV_BPL(StartX));
		}
	}

#if (APA16 && USE_APA16_HDWR)
#include "../bitblt/bitblt_apa16.h"
	/*
	 * If we are using the APA-16 hardware then we must wait for the
	 * hardware to be done with the screen.
	 */

	WAIT_QUE(tmp, 0);
#endif (APA16 && USE_APA16_HDWR)

	if (nwords > 1) {	/* more than one long wide */
		int tmpnWords = nwords;

		SrcRowBytes -= MUL_4(nwords);
		while(1) {
			do {
				*DSTBITS = *SRCBITS;
				DstBits += SIZE_OF_LONG;
				SrcBits += SIZE_OF_LONG;
			} while(--nwords > 0);

			if(--Height) {
				DstBits += DstRowBytes;
				SrcBits += SrcRowBytes;
				nwords = tmpnWords;
			} else {
				break;
			}
	    	}
	} else {		/* one long wide */
		/*
		 * Should not have subtracted MUL_4(nwords) from
		 * DstRowBytes.  So now that we know nwords == 1 we will
		 * add MUL_4(nwords) == 4 back to DstRowBytes.
		 */

		DstRowBytes += 4;

	    	while(1) {
			*DSTBITS = *SRCBITS;
			if(--Height) {
				DstBits += DstRowBytes;
				SrcBits += SrcRowBytes;
			} else {
				break;
			}
	    	}
	}
}
#endif (APA8 || APA8C)
