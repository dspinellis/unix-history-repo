/*
 * font.c 
 *
 * Copyright (c) 1985 Massachusetts Institue of Technology
 * Copyright (c) 1986 Sun Microsystems, Inc.
 * Copyright (c) 1986 David C. Martin, UC Berkeley
 *
 * David C. Martin
 * ARPA: dcmartin@ingres.Berkeley.EDU
 * UUCP: ...!ucbvax!dcmartin
 *
 * $Log:	font.c,v $
 * Revision 10.4  86/11/29  13:48:04  jg
 * fixes from Berkeley
 * 
 * Revision 1.20  86/07/27  13:46:41  dcmartin
 * more modifications to LoadVFont() to fix bugs w/ invalid pixrect pointers
 * and variable height fonts.
 * 
 * Revision 1.19  86/07/25  15:36:56  dcmartin
 * modified code in StrikeToPixfont() to free all sub-structures in 
 * FontPriv pointer.
 * 
 * Revision 1.18  86/07/25  15:30:46  dcmartin
 * changed all calls to Xalloc() to malloc() due to necessity of getting NULLs
 * when ENOMEM.
 * 
 * Revision 1.17  86/07/25  14:40:18  dcmartin
 * changed the algorithm for determining variable width.
 * should work for all fonts, now.
 * 
 * Revision 1.16  86/07/17  10:47:00  dcmartin
 * can't include <sys/param.h> on Sun release 2.x due to brain damage in
 * compiler as <sys/param.h> wants to include <sys/types.h> and so does
 * <pixrect/pixrect_hs.h> in Xsun.h
 * 
 * Revision 1.15  86/07/17  10:36:33  dcmartin
 * release version w/ new LoadXFont() and support for Sun vfont files.
 * 
 * Revision 1.14  86/07/17  10:31:49  dcmartin
 * 
 */

#ifndef lint
static char rcs_id[] = "$Header: font.c,v 10.4 86/11/29 13:48:04 jg Rel $";
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

#include "Xsun.h"
#include "vssite.h"
#include "../libvs100/param.h"
#include <vfont.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>
#include <strings.h>

/* 
 * from /usr/include/sys/param.h
 */
#define MAXPATHLEN	1024 

extern int      errno;
extern char	*getenv();
extern char	*malloc();
extern long	lseek();

#define XFONT		0
#define VFONT		1
#define ReverseShort(s)	((ReverseByte[(unsigned char)(s)]<<8) \
			| ReverseByte[(unsigned char)((s)>>8)])

static short    ReverseByte[256] = {
		0x00,0x80,0x40,0xc0,0x20,0xa0,0x60,0xe0,
		0x10,0x90,0x50,0xd0,0x30,0xb0,0x70,0xf0, 
		0x08,0x88,0x48,0xc8,0x28,0xa8,0x68,0xe8,
		0x18,0x98,0x58,0xd8,0x38,0xb8,0x78,0xf8, 
		0x04,0x84,0x44,0xc4,0x24,0xa4,0x64,0xe4,
		0x14,0x94,0x54,0xd4,0x34,0xb4,0x74,0xf4, 
		0x0c,0x8c,0x4c,0xcc,0x2c,0xac,0x6c,0xec,
		0x1c,0x9c,0x5c,0xdc,0x3c,0xbc,0x7c,0xfc, 
		0x02,0x82,0x42,0xc2,0x22,0xa2,0x62,0xe2,
		0x12,0x92,0x52,0xd2,0x32,0xb2,0x72,0xf2,
		0x0a,0x8a,0x4a,0xca,0x2a,0xaa,0x6a,0xea,
		0x1a,0x9a,0x5a,0xda,0x3a,0xba,0x7a,0xfa, 
		0x06,0x86,0x46,0xc6,0x26,0xa6,0x66,0xe6,
		0x16,0x96,0x56,0xd6,0x36,0xb6,0x76,0xf6, 
		0x0e,0x8e,0x4e,0xce,0x2e,0xae,0x6e,0xee,
		0x1e,0x9e,0x5e,0xde,0x3e,0xbe,0x7e,0xfe, 
		0x01,0x81,0x41,0xc1,0x21,0xa1,0x61,0xe1,
		0x11,0x91,0x51,0xd1,0x31,0xb1,0x71,0xf1, 
		0x09,0x89,0x49,0xc9,0x29,0xa9,0x69,0xe9,
		0x19,0x99,0x59,0xd9,0x39,0xb9,0x79,0xf9, 
		0x05,0x85,0x45,0xc5,0x25,0xa5,0x65,0xe5,
		0x15,0x95,0x55,0xd5,0x35,0xb5,0x75,0xf5, 
		0x0d,0x8d,0x4d,0xcd,0x2d,0xad,0x6d,0xed,
		0x1d,0x9d,0x5d,0xdd,0x3d,0xbd,0x7d,0xfd,
		0x03,0x83,0x43,0xc3,0x23,0xa3,0x63,0xe3,
		0x13,0x93,0x53,0xd3,0x33,0xb3,0x73,0xf3,
		0x0b,0x8b,0x4b,0xcb,0x2b,0xab,0x6b,0xeb,
		0x1b,0x9b,0x5b,0xdb,0x3b,0xbb,0x7b,0xfb,
		0x07,0x87,0x47,0xc7,0x27,0xa7,0x67,0xe7,
		0x17,0x97,0x57,0xd7,0x37,0xb7,0x77,0xf7, 
		0x0f,0x8f,0x4f,0xcf,0x2f,0xaf,0x6f,0xef,
		0x1f,0x9f,0x5f,0xdf,0x3f,0xbf,0x7f,0xff
};

extern struct pixrectops	mem_ops;
static struct mpr_data		mprd;

/*
 * These define the biggest font that will successfully load.
 */
#define MAX_FONT_WIDTH		60
#define MAX_FONT_HEIGHT		60

static struct pixrect		spr = {
					&mem_ops, 
					256 * MAX_FONT_WIDTH, 
					MAX_FONT_HEIGHT, 
					1, 
					(caddr_t) &mprd
				};

static struct pixfont *
StrikeToPixfont(fp)
FONT	*fp;
{
	register FontPriv	*fpp = (FontPriv *) fp->data;
	register struct pixfont	*pf;
	register int    	i, w;
	register short		*sh, limit;
	register struct pixchar	*pc;

	pf = (struct pixfont *) malloc(sizeof(struct pixfont));
	if (pf == (struct pixfont *) NULL) {
		errno = ENOMEM;
		return(pf);
	}
	bzero(pf, sizeof(struct pixfont));
	limit = (fpp->wpitch / 2) * fp->height;
	sh = (short *) fpp->fltable[0];
	do {
		*sh = ReverseShort(*sh);
		sh++;
	} while (--limit != -1);
	mprd.md_linebytes = fpp->wpitch;
	mprd.md_image = (short *) fpp->fltable[0];
	bzero((caddr_t) pf, sizeof(struct pixfont));
	pf->pf_defaultsize.y = fp->height;
	if (fp->fixed) {
		pf->pf_defaultsize.x = fp->avg_width;
	}
	/* for all the characters */
	for (i = fp->first; i <= fp->last; i++) {
		if ((w = fpp->widths[i]) < 0) 
			continue;
		pc = &(pf->pf_char[i]);
		pc->pc_pr = mem_create(w, fp->height, 1);
		pc->pc_home.x = 0;
		pc->pc_home.y = fp->height - fp->base;
		pc->pc_adv.x = w;
		pc->pc_adv.y = 0;
		pr_rop(pc->pc_pr, 0, 0, w, fp->height, PIX_SRC,
		       &spr, fpp->leftarray[i], 0);
	}
	free((caddr_t) fpp->leftarray);
	free((caddr_t) fpp->widths);
	free((caddr_t) fpp->strike);
	free((caddr_t) fpp->fltable);
	free((caddr_t) fpp);
	return(pf);
} /* end StrikeToPixfont() */

/*
 * LoadXFont - load an xfont format file
 *
 * The FontData structure consists of {
 *	short	f_characters[5]
 *	short	f_firstChar
 *	short	f_lastChar
 *	short	f_leftArray[2]
 *	short	f_baseline
 *	short	f_spaceIndex
 *	short	f_fixedWidth
 * }
 *
 * The five shorts of f_characters[] are:
 *	[0] - location (in bytes) from start of file where font data begins
 *	[1] - unused
 *	[2] - width of font bitmap pointed at by [0]
 *	[3] - height of font bitmap
 *	[4] - bits per pixel of font bitmap
 *
 * The two shorts of f_leftArray[] are:
 *	[0] - location (in bytes) from start of file of bitmap offset array
 *	[1] - unused
 *
 */
static FONT *
LoadXFont(fildes, name)
int	fildes;
caddr_t	name;
{
	register int	i, j;
	int		font_nbytes, totalWidth, datasiz, tablesiz;
	char		*fontdata;
	short		*offset_table, *width_table;
	FONT		*fp;
	FontData	font;
	FontPriv	*fpriv;
	struct stat	stb;

	/* read in the xfont header */
	if (read(fildes, (caddr_t) &font, sizeof(FontData)) != 
		sizeof(FontData)) {
		errno = EINVAL;
		return (NULL);
	}
	/* byte swap */
	Swap_shorts((short *) &font, sizeof(FontData) / sizeof(short));
	/* determine the number of bytes in the bitmap */
	font_nbytes = BitmapSize(font.f_characters[2], font.f_characters[3]);
	/* stat the file to determine size */
	fstat(fildes, &stb);
	/* if the location of the offset table is > location of bitmap ... */
	if (font.f_leftArray[0] > font.f_characters[0]) {
		/* check for sufficient space for font bitmap */
		if (font.f_leftArray[0] - font.f_characters[0] < font_nbytes) {
			/* no */
			errno = EINVAL;
			return(NULL);
		}
		/* determine the space allocated for the offset table */
		tablesiz = stb.st_size - font.f_leftArray[0];
		/* determine the space allocated for the font data */
		datasiz = font.f_leftArray[0] - font.f_characters[0];
	} else {
		/* check for sufficient space for font bitmap */
		if (font_nbytes + font.f_characters[0] > stb.st_size) {
			errno = EINVAL;
			return(NULL);
		}
		/* determine the space allocated for the offset table */
		tablesiz = font.f_characters[0] - font.f_leftArray[0];
		/* determine the space allocated for the font data */
		datasiz = stb.st_size - font.f_characters[0];
	}
	/* are we going to ask for more than we have? */
	if (font_nbytes > datasiz) {
		errno = EINVAL;
		return(NULL);
	}
	/* allocate space for the font bitmap */
	fontdata = (caddr_t) malloc(font_nbytes);
	if (fontdata == (caddr_t) NULL) {
		errno = ENOMEM;
		return(NULL);
	}
	/* seek to start of bitmap */
	lseek(fildes, (long) font.f_characters[0], 0);
	/* read it in */
	if (read(fildes, fontdata, font_nbytes) != font_nbytes) {
		free(fontdata);
		errno = EINVAL;
		return (NULL);
	}
	/* byte swap */
	Swap_shorts((short *) fontdata, font_nbytes / sizeof(short));
	/* is there an offset table? */
	if (tablesiz == 0) {
		/* no -- this had better be a fixed width font */
		if (font.f_fixedWidth == 0) {
			free(fontdata);
			errno = EINVAL;
			return(NULL);
		}
		/* determine table size */
		tablesiz = sizeof(short) * 
		    (font.f_lastChar - font.f_firstChar + 2);
		/* allocate space for table */
		offset_table = (short *) malloc(tablesiz);
		if (offset_table == (short *) NULL) {
			free(fontdata);
			errno = ENOMEM;
			return(NULL);
		}
		/* zero out the offset table */
		bzero(offset_table, tablesiz);
		/* build the offset table */
		for (j=0, i = font.f_firstChar; i < font.f_lastChar + 2; i++) {
			offset_table[i] = j;
			j += font.f_fixedWidth;
		}
	} else {
		/* allocate space for table */
		offset_table = (short *) malloc(tablesiz);
		if (offset_table == (short *) NULL) {
			free(fontdata);
			errno = ENOMEM;
			return(NULL);
		}
		/* zero out the offset table */
		bzero(offset_table, tablesiz);
		/* seek to offset array */
		lseek(fildes, (long) font.f_leftArray[0], 0);
		/* read the offset array */
		if (read(fildes, (caddr_t) &offset_table[font.f_firstChar], 
		    tablesiz) != tablesiz) {
			free(fontdata);
			free((caddr_t) offset_table);
			errno = EINVAL;
			return (NULL);
		}
		Swap_shorts(offset_table, tablesiz / sizeof(short));
	}
	/* allocate a width table -- to be built from the offset table */
	width_table = (short *) malloc(tablesiz);
	if (width_table == (short *) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		errno = ENOMEM;
		return(NULL);
	}
	/* allocate space for the device dependent private font data */
	fpriv = (FontPriv *) malloc(sizeof(FontPriv));
	if (fpriv == (FontPriv *) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		free((caddr_t) width_table);
		errno = ENOMEM;
		return(NULL);
	}
	/* initialize */
	fpriv->maxwidth = 0;
	totalWidth = 0;
	/* convert the offset table to the width table */
	for (i = font.f_firstChar; i <= font.f_lastChar; i++) {
		width_table[i] = offset_table[i + 1] - offset_table[i];
		if (width_table[i] > fpriv->maxwidth) {
			fpriv->maxwidth = width_table[i];
		}
		if (width_table[i] < 0) {
			errno = EINVAL;
			return(NULL);
		}
		totalWidth += width_table[i];
	}
	fpriv->widths = width_table;
	fpriv->leftarray = offset_table;
	/* allocate space for the strike font */
	fpriv->strike = (BITMAP *) malloc(sizeof(BITMAP));
	if (fpriv->strike == (BITMAP *) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		free((caddr_t) width_table);
		free((caddr_t) fpriv);
		errno = ENOMEM;
		return(NULL);
	}
	/* determine the number of bytes per line of the bitmap */
	fpriv->wpitch = (((font.f_characters[2] + 15) >> 3) & ~1);
	/* set other strike stuff .. */
	fpriv->strike->width = font.f_characters[2];
	fpriv->strike->height = font.f_characters[3];
	fpriv->strike->refcnt = 1;
	fpriv->strike->data = (caddr_t) fontdata;
	/* allocate space for the font line table */
	fpriv->fltable = (char **) malloc(font.f_characters[3]*sizeof(char *));
	if (fpriv->fltable == (char **) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		free((caddr_t) width_table);
		free((caddr_t) fpriv->strike);
		free((caddr_t) fpriv);
		errno = ENOMEM;
		return(NULL);
	}
	/* compute the font line table entries */
	for (i = 0; i < font.f_characters[3]; i++) {
		fpriv->fltable[i] = ((caddr_t) fontdata) + i * fpriv->wpitch;
	}
	/* allocate an xfont */
	fp = (FONT *) malloc(sizeof(FONT));
	if (fp == (FONT *) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		free((caddr_t) width_table);
		free((caddr_t) fpriv->strike);
		free((caddr_t) fpriv->fltable);
		free((caddr_t) fpriv);
		errno = ENOMEM;
		return(NULL);
	}
	fp->first = font.f_firstChar;
	fp->last = font.f_lastChar;
	fp->height = font.f_characters[3];
	fp->base = font.f_baseline;
	fp->space = font.f_spaceIndex + font.f_firstChar;
	fp->refcnt = 1;
	/* set the average width */
	fp->avg_width = totalWidth / (fp->last - fp->first + 1);
	if (fp->avg_width == font.f_fixedWidth) {
		fp->fixed = 1;
		fpriv->maxwidth = fp->avg_width;
	} else {
		fp->fixed = 0;
	}
	/* allocate space for the name */
	fp->name = (caddr_t) malloc(strlen(name) + 1);
	if (fp->name == (caddr_t) NULL) {
		free(fontdata);
		free((caddr_t) offset_table);
		free((caddr_t) width_table);
		free((caddr_t) fpriv->fltable);
		free((caddr_t) fpriv->strike);
		free((caddr_t) fpriv);
		free((caddr_t) fp);
		errno = ENOMEM;
		return(NULL);
	}
	/* copy the name */
	(void) strcpy(fp->name, name);
	/* change private font to pixfont */
	fp->data = (caddr_t) fpriv;
	fp->data = (caddr_t) StrikeToPixfont(fp);
	return (fp);
} /* end LoadXFont() */

static FONT *
LoadVFont(fd, name)
int		fd;
caddr_t		name;
{
	register int		i;
	int			fixed = 1;
	register struct pixfont	*pf = (struct pixfont *) NULL;
	register struct pixchar	*pc;
	FONT			*fp;
	FILE			*fonts;
	short			magic;
	struct header		hd;
	struct dispatch		disp[NUM_DISPATCH];
	struct stat		stb;
	int			maxheight, maxwidth, maxup, maxdown;

#define	fbase (sizeof hd + sizeof disp)

	fonts = fdopen(fd, "r");
	if (fonts == 0)
		return (0);
	if (fread(&hd, sizeof hd, 1, fonts) != 1 ||
	    fread(disp, sizeof disp, 1, fonts) != 1)
		goto bad;
	if (hd.magic != VFONT_MAGIC)
		goto bad;
	fstat(fd, &stb);
	if (stb.st_size != fbase + hd.size)
		goto bad;

	/* 
	 * Allocate font header and set default sizes. The default width of the
	 * font is taken to be the width of a lower-case a, if there is one.
	 * The default interline spacing is taken to be 3/2 the height of an
	 * upper-case A above the baseline.
	 */
	pf = (struct pixfont *) malloc(sizeof(struct pixfont));
	if (pf == (struct pixfont *) NULL)
		goto bad;
	/* zero out the pixfont */
	bzero(pf, sizeof(struct pixfont));
	/* initialize */
	maxwidth = maxup = maxdown = 0;
	/* determine maximum width, up and down for font */
	for (i = 0; i < NUM_DISPATCH; i++) {
		register struct dispatch	*d = &disp[i];

		if (d->left + d->right > maxwidth)
			maxwidth = d->left + d->right;
		if (d->up > maxup)
			maxup = d->up;
		if (d->down > maxdown)
			maxdown = d->down;
	}
	maxheight = maxup + maxdown;
	pf->pf_defaultsize.x = maxwidth;
	pf->pf_defaultsize.y = maxheight;
	/* Create memory pixrects for characters of font. */
	for (i = 0; i < NUM_DISPATCH; i++) {
		register struct dispatch	*d = &disp[i];
		struct pr_size			size;
		struct pixrect			*cpr;
		int				j, wb, ww;
		short				*sp;

		/* pointer to character */
		pc = &pf->pf_char[i];
		/* if there is no glyph, there is no character */
		if (d->nbytes == 0) {
			pc->pc_pr = (struct pixrect *) NULL;
			pc->pc_home.x = 0;
			pc->pc_home.y = 0;
			pc->pc_adv.x = 0;
			pc->pc_adv.y = 0;
			continue;
		}
		/* total character width */
		size.x = d->left + d->right;
		/* total character height */
		size.y = d->up + d->down;
		/* check for variable width */
		if (size.x != 0 && pf->pf_defaultsize.x != size.x)
			fixed = 0;
		/* get width in bytes and words */
		wb = (size.x + 7) >> 3;
		ww = (size.x + 15) >> 4;
		/* create a memory pixrect for character */
		if ((cpr = mem_create(size, 1)) == (struct pixrect *) NULL)
			goto bad;
		/* pointer to pixrect data */
		sp = mpr_d(cpr)->md_image;
		/* seek to character */
		fseek(fonts, fbase + d->addr, 0);
		/* read in the bytes */
		for (j = 0; j < size.y; j++) {
			fread((caddr_t) sp, wb, 1, fonts);
			sp += ww;
		}
		/* check to make sure all characters are the same height */
		if (size.y != maxheight) {
			register struct pixrect	*npr;

			npr = mem_create(size.x, maxheight, 1);
			if (npr == (struct pixrect *) NULL)
				goto bad;
			d->up = maxup - d->up;
			pr_rop(npr, 0, d->up, size.x, maxheight,
				PIX_SRC, cpr, 0, 0);
			(void) pr_destroy(cpr);
			cpr = npr;
		}
		/* set character pixrect pointer */
		pc->pc_pr = cpr;
		/* home from left edge */
		pc->pc_home.x = -d->left;
		/* home from baseline */
		pc->pc_home.y = -d->up;
		/* character horizonal advance */
		pc->pc_adv.x = d->width;
		/* character vertical advance (vfont == 0) */
		pc->pc_adv.y = 0;
	}
	goto good;
bad:
	free(pf);
	errno = EINVAL;
	return(NULL);
good:
	fclose(fonts);

	/* allocate space for the X font description */
	if ((fp = (FONT *) malloc(sizeof(FONT))) == (FONT *) NULL) {
		free((caddr_t) pf);
		errno = ENOMEM;
		return(NULL);
	}
	/* allocate space for font name */
	if ((fp->name = malloc(strlen(name) + 1)) == NULL) {
		free((caddr_t) pf);
		free((caddr_t) fp);
		errno = ENOMEM;
		return(NULL);
	}
	/* copy the name */
	(void) strcpy(name, fp->name);
	/* set the X font attributes */
	if (fixed == 1) {
		fp->fixed = 1;
		fp->avg_width = pf->pf_defaultsize.x;
	} else {
		fp->fixed = 0;
		fp->avg_width = 0;
	}
	fp->first = 0;
	fp->last = 255;
	fp->space = 32;
	fp->height = maxheight;
	fp->base = maxdown;
	fp->refcnt = 1;
	fp->data = (caddr_t) pf;
	return(fp);
} /* end LoadVFont() */

static int
OpenFile(FileName, type_ptr)
char	*FileName;
int	*type_ptr;
{
	int		fd;
	struct header	hdrbuf;

	/* open the file */
	if ((fd = open(FileName, O_RDONLY, 0)) < 0) {
		return(-1);
	}	
	/* read header buffer */
	if (read(fd, (caddr_t) &hdrbuf, sizeof(struct header)) == 
		sizeof(struct header)) {
		/* read \something/ -- now check the magic number */
		if (hdrbuf.magic == VFONT_MAGIC) {
			/* VFONT */
			*type_ptr = VFONT;
		} else {
			/* XFONT */
			*type_ptr = XFONT;
		}
		/* rewind */
		(void) lseek(fd, (long) 0, 0);
		/* return the file descriptor */
		return(fd);
	}
	/* bad read -- close and return -1 */
	close(fd);
	return(-1);
} /* end OpenFile() */

static caddr_t
SearchXFontPath(fname)
register caddr_t	fname;
{
	register caddr_t	cp, pp, c2p;
	static char		xfontpath[MAXPATHLEN];

	/* check for an absolute pathname */
	if (*fname == '/') {
		if (access(fname, R_OK) == 0)
			return(fname);
		else
			return((caddr_t) NULL);
	}

	/* get the environment variable for XFONTPATH */
	if ((pp = getenv("XFONTPATH")) == (caddr_t) NULL) {
		pp = DEFAULT_FONT_PATH;
	}

	for (; *pp != '\0'; pp++) {
		for (cp = xfontpath; *pp != ':' && *pp != '\0'; *cp++ = *pp++);
		*cp++ = '/';
		for (c2p = fname; *c2p != '\0'; *cp++ = *c2p++);
		for (c2p = DEFAULT_FONT_SUFFIX; *c2p != '\0'; *cp++ = *c2p++);
		*cp = '\0';
		if (access(xfontpath, R_OK) == 0) {
			return(xfontpath);
		}
	}
	return ((caddr_t) NULL);
} /* end SearchXFontPath() */

static int
FindFile(name, font_type)
char	*name;
int	*font_type;
{
	caddr_t		pathname;
	int		fd;

	if ((pathname = SearchXFontPath(name)) == (caddr_t) NULL) {
		return(-1);
	}
	if ((fd = OpenFile(pathname, font_type)) < 0) {
		return(-1);
	}
	return(fd);
} /* end FindFile() */

extern FONT *
GetFont(name)
caddr_t	name;
{
	int	fd, type;
	FONT	*fp;

	if ((fd = FindFile(name, &type)) < 0) {
		errno = EINVAL;
		return ((FONT *) NULL);
	}

	switch (type) {
	case VFONT: 
		fp = LoadVFont(fd, name); 
		break;
	case XFONT: 
		fp = LoadXFont(fd, name); 
		break;
	default:
		close(fd);
		errno = EINVAL;
		return((FONT *) NULL);
	} /* end switch */
	close(fd);
	if (fp == (FONT *) NULL) {
		errno = EINVAL;
		return((FONT *) NULL);
	}
	return(fp);
} /* end GetFont() */

FreeFont(font)
FONT	*font;
{
	pf_close((caddr_t) font->data);
	free(font->name);
	free((caddr_t) font);
}

#endif sun
