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
static char sccsid[] = "@(#)font.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright 1985, Massachusetts Institute of Technology
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* font.c	Reads a font from a file and stores it on the workstation
 *
 *	GetFont		Takes a font name and stores it
 *	FreeFont	Frees the storage taken by a font
 *
 */

/*
 *	ToDo:
 *		Use Sun fonts too
 */

#include "Xsun.h"
#include "vssite.h"
#include "../libvs100/param.h"
#include <errno.h>

extern int errno;

char *Xalloc(), *strcpy(), *strcat();
long lseek();

#define CHARPERFONT 256

static short ReverseBitsInByte[256] = {
0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0, 0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8, 0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4, 0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec, 0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2, 0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea, 0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6, 0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee, 0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1, 0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9, 0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5, 0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed, 0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3, 0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb, 0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7, 0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef, 0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff,
};

#define ReverseBitsInShort(s) ((ReverseBitsInByte[(unsigned char)(s)]<<8)|ReverseBitsInByte[(unsigned char)((s)>>8)])

/* 
 * this routine converts strike format into Sun font format
 * XXX - it's embarrasingly slow,  but I'm in too much of a hurry
 * 	to do anything better for now.  The font stuff is for the deep
 *	six anyway..... (DSHR).
 */
struct pixfont *
striketopixfont(fn)
	FONT *fn;
{
    register FontPriv *fp = (FontPriv *)fn->data;
    register struct pixfont *pf;
    register int         fheight = fn->height;
    register int         i, j;

    ((struct pixfont *)fn->data) = pf = (struct pixfont *) Xalloc(sizeof(struct pixfont));
    if (pf != NULL) {
	extern struct pixrectops mem_ops;
	static struct mpr_data mprd;
	static struct pixrect spr = {&mem_ops,2000,1000,1,(caddr_t)&mprd};
	{   register short *sh = (short *) fp->fltable[0];
	    register short limit = (fp->wpitch>>1)*fheight;
	    do *sh = ReverseBitsInShort(*sh), sh++;
	    while (--limit != -1);
	}
	mprd.md_linebytes = fp->wpitch;
	mprd.md_image = (short *) fp->fltable[0];
	bzero((caddr_t) pf, sizeof(struct pixfont));
	pf->pf_defaultsize.y = fheight;
	if (fn->fixed)
	    pf->pf_defaultsize.x = fn->avg_width;
	for (i = fn->first; i <= fn->last; i++) {
	    register    w = fp->widths[i];
#ifdef undef
	    register    offset = fp->leftarray[i];
#endif
	    register struct pixchar *pc = &(pf->pf_char[i]);

	    if (w < 0)
		continue;	/* Sanity check */
	    pc->pc_pr = mem_create(w, fheight, 1);
	    pc->pc_home.x = 0;
	    pc->pc_home.y = fheight - fn->base;
	    pc->pc_adv.x = w;
	    pc->pc_adv.y = 0;
	    pr_rop(pc->pc_pr,0,0,w,fheight,PIX_SRC,
		   &spr, fp->leftarray[i], 0);
#ifdef undef
	    for (j = 0; j < fheight; j++) {
		register int k;
		for (k = 0; k < w; k++) {
		    int bit;

		    bit = (*((u_short *)fp->fltable[j] + ((offset+k) >> 4)));
		    bit >>= ((offset + k) % 16);
		    bit &= 1;
		    pr_put(pc->pc_pr, k, j, bit);

		}
	    }
#endif
	}
    }
    free((caddr_t) fp);
    return ((struct pixfont *)fn->data);
}

FONT *GetFont (name)
	char *name;
{
	char fontname[256];
	int fontfile = -1;
	FontData font;
#define chars ((BitMap *) font.f_characters)
	int fontsize, leftsize, width, i, j;
	char *fontarea;
	register short *leftarea, *leftarray;
	register FONT *fd;
	register FontPriv *fpriv;
	int tablesize = (CHARPERFONT + 1) * sizeof(short);
	extern char *getenv(), *index();
	static char *Fontpath = NULL;
	char *fontpath;

	if (!Fontpath) {
	    char *temp = getenv("XFONTPATH");

	    if (temp) {
		Fontpath = Xalloc(strlen(temp) + 1);
		strcpy(Fontpath, temp);
	    }
	}
	
	if ((fontpath = Fontpath) == NULL)
	    fontpath = DEFAULT_FONT_PATH;

	fontfile = open (name, 0, 0);

	while (fontfile < 0) {
	    char *fend;

	    if ((fend = index(fontpath, ':')) != 0)
		*fend = '\0';
	    if (*fontpath == '\0') {
		errno = EINVAL;
		return (NULL);
	    }
	    
	    if (*fontpath == '~') {
		/* XXX - should implement ~foobar as well */
		strcpy (fontname, getenv("HOME"));
		strcat (fontname, "/");
		fontpath++;
	    } else
	        *fontname = '\0';
	    strcat (fontname, fontpath);
	    strcat (fontname, "/");
	    strcat (fontname, name);
	    strcat (fontname, DEFAULT_FONT_SUFFIX);

	    fontfile = open (fontname, 0, 0);
	    if (fend) {
		*fend = ':';
	        fontpath = ++fend;
	    } else
	        break;
	}

	if (read (fontfile, (caddr_t) &font, sizeof (FontData)) != sizeof (FontData)) {
	    close (fontfile);
	    errno = EINVAL;
	    return (NULL);
	}
	Swap_shorts((short *) &font, sizeof (FontData)>>1);

	fontsize = BitmapSize(chars->bm_width, chars->bm_height);
	fontarea = (char *) Xalloc (fontsize);
	lseek (fontfile, (long) font.f_characters[0], 0);
	if (read (fontfile, fontarea, fontsize) != fontsize) {
	    close (fontfile);
	    free (fontarea);
	    errno = EINVAL;
	    return (NULL);
	}
	Swap_shorts((short *)fontarea, fontsize>>1);

	leftarea  = (short *) Xalloc (tablesize);
	bzero(leftarea, tablesize);
	leftarray = (short *) Xalloc (tablesize);
	if (font.f_fixedWidth == 0) {
	    leftsize = (font.f_lastChar - font.f_firstChar + 2) * sizeof (short);
	    lseek (fontfile, (long) font.f_leftArray[0], 0);
	    if (read (fontfile, & leftarea[font.f_firstChar], leftsize) 
	    		!= leftsize) {
		close (fontfile);
		free (fontarea);
		free ((caddr_t) leftarea);
		free ((caddr_t) leftarray);
		errno = EINVAL;
		return (NULL);
	    }
	    Swap_shorts(((short *)& leftarea[font.f_firstChar]), leftsize>>1);
	} else { /* if fixed with font, generate leftarray for use later */
	    j = 0;
	    for (i = font.f_firstChar; i <= font.f_lastChar + 1; i++) {
		leftarea[i] = j;
		j += font.f_fixedWidth;
	    }
	}
	bcopy(leftarea, leftarray, tablesize);

	close (fontfile);

	fd = (FONT *) Xalloc (sizeof (FONT));

	fd->height = chars->bm_height;
	fd->first = font.f_firstChar;
	fd->last = font.f_lastChar;

	fd->base = font.f_baseline;
	fd->space = font.f_spaceIndex;
	fd->space += fd->first;
	fpriv = (FontPriv *) Xalloc (sizeof (FontPriv));
	if (fd->avg_width = font.f_fixedWidth) {
	    fd->fixed = 1;
	    fpriv->maxwidth = fd->avg_width;
	    }
	else
	    fd->fixed = 0;

	fd->refcnt = 1;
	fd->data = (caddr_t) fpriv;
	fpriv->widths = leftarea;
	fpriv->leftarray = leftarray;

	if ((fpriv->strike = (BITMAP *) Xalloc(sizeof(BITMAP))) == NULL) {
	    free (fontarea);
	    free ((caddr_t) leftarea);
	    free ((caddr_t) leftarray);
	    free ((caddr_t) fd);
	    free ((caddr_t) fpriv);
	    errno = EINVAL;
	    return (NULL);
	}
	fpriv->wpitch = (((chars->bm_width + 15) >> 3) & ~1);
	fpriv->strike->width = chars->bm_width;
	fpriv->strike->height = chars->bm_height;
	fpriv->strike->refcnt = 1;
	fpriv->strike->data = (caddr_t) fontarea;
	/*
	 * compute line table for font to eliminate multiply to find beginning
	 * of line.
	 */
	fpriv->fltable = (char **)Xalloc(chars->bm_height * sizeof(caddr_t));
	for (i = 0; i < chars->bm_height; i++) 
		fpriv->fltable[i] = ((caddr_t) fontarea) + i * fpriv->wpitch;

	fd->name = (char *) Xalloc (strlen (name) + 1);
	strcpy (fd->name, name);

	fpriv->maxwidth = 0;

	/* convert the leftarray to the width table */
	for (i = fd->first; i <= fd->last; i++) {
	    width = fpriv->leftarray[i + 1] - fpriv->leftarray[i];
	    if (width > fpriv->maxwidth) fpriv->maxwidth = width;
	    if (width < 0) {
		width = 0;	/* font sanity check */
#ifdef	notdef
		DeviceError ("Bad font leftarray!\n");
#else
		errno = EINVAL;
		return(NULL);
#endif
		}
	    fpriv->widths[i] = width;
	}
		
        fd->avg_width = ((fpriv->leftarray[fd->last]  - 
		fpriv->leftarray[fd->first]) / (fd->last - fd->first));

	striketopixfont(fd);
	return (fd);
#undef chars
}

FreeFont (font)
	register FONT *font;
{
	free ((caddr_t) font->data);
	free (font->name);
	free ((caddr_t) font);
}
#endif	sun
