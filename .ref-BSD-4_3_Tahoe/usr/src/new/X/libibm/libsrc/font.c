#ifndef lint
static char *rcsid_font_c = "$Header: font.c,v 10.1 86/11/19 10:41:45 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* font.c - Reads a font from a file and stores it on the workstation
 *
 *      GetFont         Takes a font name and opens it
 *      FreeFont        Frees the storage taken by a font
 *	StrikeToBitmaps converts strike format into an array of individual
 *			character bitmaps
 *
 *  	Changes and additions by:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *     		Providence, RI 02912
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
#include "xsite.h"
#include "font.h"

/*
 * Open font file
 */

FONT *GetFont (name)
        char *name;
{
        char fontname[256];
        int fontfile;
        FontData fd;
#define chars ((BitMap *) fd.f_characters)
        int fontsize, leftsize, width;
	register i, j;
	BITMAP *strike_bm;
        char *fontarea;
	int VisibleChars = 0;
        register short *leftarea, *leftarray;
        register FONT *font;
        register FontPriv *fpriv;
        int tablesize = CHARPERFONT * sizeof(short);

#ifdef TRACE_X
	fprintf(stderr, "In GetFont\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Convert font name into full path name
	 */

        strcpy (fontname, DEFAULT_FONT_DIRECTORY);
        strcat (fontname, name);
        strcat (fontname, DEFAULT_FONT_SUFFIX);

	/*
	 * Open font file
	 */

        if ((fontfile = open (fontname, 0)) == -1 &&
	    (errno != ENOENT || (fontfile = open (name, 0)) == -1)) {
		errno = EINVAL;
		return (NULL);
        }

	/*
	 * Read in font data structure
	 */

        if (read (fontfile, (caddr_t) &fd, sizeof (FontData))
	    != sizeof(FontData)) {
        	close (fontfile);
        	errno = EINVAL;
        	return (NULL);
        }

	/*
	 * Swap each of the shorts in font data structure.
	 * font was created on a VAX and needs to be swapped 
	 * for this hardware.
	 */

	Swap_shorts((short *) &fd, sizeof(FontData) / sizeof(short));

	/*
	 * Allocate space for font bitmap.
	 * bitmap is in strike format.
	 */

        fontsize = BitmapSize(chars->bm_width, chars->bm_height);
        fontarea = (char *) Xalloc (fontsize);

	/*
	 * Read font bitmap into allocated area
	 */

        lseek (fontfile, (long) fd.f_characters[0], 0);
        if (read (fontfile, fontarea, fontsize) != fontsize) {
		close (fontfile);
    		free (fontarea);
    		errno = EINVAL;
    		return (NULL);
        }

	/*
	 * Reverse all the bits in each character of font bitmap.
	 * The font bitmap was created on VAX and needs to be
	 * reversed for this hardware.
	 */

	ReverseCharBits(fontarea, fontsize);

	/*
	 * Allocate space for left array and width table
	 */

        leftarea  = (short *) Xalloc (tablesize);
        bzero((caddr_t)leftarea, tablesize);
        leftarray = (short *) Xalloc (tablesize);

	/*
	 * What kind of font is this ?
	 */

        if (fd.f_fixedWidth == 0) {
		/*
		 * The font is variable width so allocate space for left
		 * array and read it in. The left array is an array
		 * of pointers to the left most bit of each character
		 * in the font.
		 */

		leftsize = (fd.f_lastChar - fd.f_firstChar + 2)
			   * sizeof (short);
		lseek (fontfile, (long) fd.f_leftArray[0], 0);
		if (read(fontfile,(caddr_t)&leftarea[fd.f_firstChar],leftsize) 
		    != leftsize) {
			close (fontfile);
			free (fontarea);
			free ((caddr_t) leftarea);
			free ((caddr_t) leftarray);
			errno = EINVAL;
			return (NULL);
		}

		/*
		 * Swap each short in the leftarray. The array was created 
		 * on a VAX and needs to be swapped for this hardware
		 */

	 	Swap_shorts(leftarea, leftsize / sizeof (short));
        } else	{
		/*
		 * The font is of a fixed width so create the left
		 * array ourselves
		 */

		j = 0;
        	for (i = fd.f_firstChar; i <= fd.f_lastChar + 1; i++) {
        		leftarea[i] = j;
        		j += fd.f_fixedWidth;
        	}
        }

	/*
	 * Make a copy of the left array for future use
	 */

        bcopy(leftarea, leftarray, tablesize);

	/*
	 * Close the font file
	 */

        close (fontfile);

	/*
	 * Allocate space for FONT structure
	 */

        font = (FONT *) Xalloc (sizeof (FONT));

	/*
	 * Fill in FONT structure with data obtained from font file
	 */

        font->height = chars->bm_height;
        font->first = fd.f_firstChar;
        font->last = fd.f_lastChar;
        font->base = fd.f_baseline;
        font->space = fd.f_spaceIndex;
        font->space += font->first;
        if (fd.f_fixedWidth) {
        	font->fixed = 1;
        } else	{
        	font->fixed = 0;
	}
        font->refcnt = 1;
        font->name = (char *) Xalloc (strlen (name) + 1);
        strcpy (font->name, name);

	/*
	 * Allocate space for the fonts private data structure
	 */

        fpriv = (FontPriv *) Xalloc (sizeof (FontPriv));
        font->data = (caddr_t) fpriv;

	/*
	 * Save pointers to left array, width table , and font bitmap
	 */

        fpriv->widths = leftarea;
        fpriv->leftarray = leftarray;

	/*
	 * Allocate a temporary buffer for the strike bitmap.
	 */
        if ((strike_bm = (BITMAP *) Xalloc(sizeof(BITMAP))) == NULL) {
		free (fontarea);
		free ((caddr_t) leftarea);
		free ((caddr_t) leftarray);
		free ((caddr_t) name);
		free ((caddr_t) font);
		free ((caddr_t) fpriv);
		return (NULL);
        }

        strike_bm->width = chars->bm_width;
        strike_bm->height = chars->bm_height;
        strike_bm->refcnt = 1;
        strike_bm->data = (caddr_t) fontarea;

        /*
	 * Convert leftarray to the width table
	 */

        fpriv->maxwidth = 0;
        for (i = font->first; i < font->last; i++) {
        	width = fpriv->leftarray[i + 1] - fpriv->leftarray[i];
        	if (width > fpriv->maxwidth) {
			fpriv->maxwidth = width;
		}
		if (width < 0) {
			width = 0;      /* font sanity check */
			DeviceError ("Bad font leftarray!");
        	} else	if (width > 0) {
			VisibleChars++;
		}
        	fpriv->widths[i] = width;
        }
	fpriv->widths[i] = strike_bm->width - fpriv->leftarray[i];
	font->avg_width = strike_bm->width / VisibleChars;

	/*
	 * Make individual bitmaps of each character in font
	 */

        if (StrikeToBitmaps(font,strike_bm) == NULL) {
		free (fontarea);
		free ((caddr_t) leftarea);
		free ((caddr_t) leftarray);
		free ((caddr_t) strike_bm);
		free ((caddr_t) fpriv);
		free ((caddr_t) name);
		free ((caddr_t) font);
		return (NULL);
	}

	/*
	 * Set up offscr if the font will fit into the offscreen
	 * buffer.
	 */

	if (font->height > MAX_OFFSCR_HT) {
		fpriv->offscr = NILBITMAP;
	} else {
		fpriv->offscr = &txtbm;
	}

	/*
	 * Don't need the temporary strike bitmap.
	 */

	free ((caddr_t) strike_bm);

	/*
	 * Return pointer to FONT stucture
	 */

        return (font);
#undef chars
}

/*
 * Free all allocated space used by font
 */

FreeFont (font)
        register FONT *font;
{
        register FontPriv *data;

#ifdef TRACE_X
	fprintf(stderr, "In FreeFont\n");
	fflush(stderr);
#endif TRACE_X

        data = FDATA(font);
        if (data->widths) {
		free ((caddr_t) data->widths);
	}
        FreeBitmap(data->chrs);
        free ((caddr_t) data);
        free (font->name);
        free ((caddr_t) font);
}

/*
 * This routine converts strike format into an array of bitmaps
 */

StrikeToBitmaps(font,sbm)
        FONT *font;
	BITMAP *sbm;
{
        register FontPriv *fpriv = FDATA(font);
	register BITMAP *cbm;
	register charwidth;
	register charoffset;
	register i;
	register desty = 0;
        int size; 

#ifdef TRACE_X
	fprintf(stderr, "In StrikeToBitmaps\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Allocate bitmap structure for character bitmap
	 */

        if ((cbm = fpriv->chrs = (BITMAP *) Xalloc(sizeof(BITMAP))) == NULL) {
		return(NULL);
	}

	/*
	 * Allocate all of the individual character bitmaps in one shot
	 */

	cbm->width = (((fpriv->maxwidth + 15) >> 4) << 4);
	cbm->height = (font->last - font->first + 1) * font->height;
	size = BitmapSize(cbm->width, cbm->height);
	cbm->refcnt = 1;
	if ((cbm->data = (char *) Xalloc(size)) == NULL) {
		free((caddr_t) cbm);
		return(NULL);
	}
        bzero(cbm->data, size);

	/*
	 * Loop thru font and blt each character into its 
	 * apporpriate bitmap
	 */

        for (i = font->first; i <= font->last; i++) {

		charwidth = fpriv->widths[i];
		charoffset = fpriv->leftarray[i];

               /*
                 * make source and destination rectangles
                 */

                FillInRect(charoffset, 0, charwidth, font->height, &SrcRect);
                FillInRect(0, desty, charwidth, font->height, &DstRect);

                /*
                 * blt character to bitmap
                 */

                CopyBits((u_short *)sbm->data,sbm->width,sbm->height, &SrcRect,
                         (u_short *)cbm->data,cbm->width,cbm->height, &DstRect,
                         NILMASK, NIL, NIL, GXcopy, 0, NILCLIP);

                /*
                 * adjust destination address
                 */

                desty += font->height;
        }
	
	return(1);
}
