/*
 *	$Source: /u1/X/libis/RCS/font.c,v $
 *	$Header: font.c,v 1.1 86/11/17 14:34:11 swick Rel $
 */

#ifndef lint
static char *rcsid_font_c = "$Header: font.c,v 1.1 86/11/17 14:34:11 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	font.c
 *
 *	GetFont		Takes a font name and stores it
 *	FreeFont	Frees the storage taken by a font
 *	MakeFontPixmap 	Convert font bitmap to pixmap
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"
#include "vssite.h"
#include <errno.h>
#include <sys/file.h>

extern int errno;

extern char *strcpy();
extern char *strcat();
extern long lseek();
extern PIXMAP *MakePixmap();

/*
 * BitMap and FontData typedefs come from ../libvs100/param.h
 */

/* BitMap typedefs */
typedef short a_BitmapEntryPtr[2];

typedef struct _Bitmap {
    a_BitmapEntryPtr bm_address;
    short bm_width;
    short bm_height;
    short bm_bitsPerPixel;
} BitMap;

typedef short a_Bitmap[5];

/* FontData typedefs */
typedef short a_FontWidthEntryPtr[2];

typedef struct _FontData {
    a_Bitmap	f_characters;
    short	f_firstChar;
    short	f_lastChar;
    a_FontWidthEntryPtr f_leftArray;
    short	f_baseline;
    short	f_spaceIndex;
    short	f_fixedWidth;
} FontData;

/*
 *	GetFont
 */
FONT *GetFont(name)
char	*name;
{
    char pathname[1024];	/* font pathname			*/
    int file;			/* file descriptor			*/
    FontData hdr;		/* font file header			*/
#define chars ((BitMap *) hdr.f_characters)
    int mask_size;		/* size of mask bitmap			*/
    int xpos_size;		/* size of x pos array			*/
    char *mask;			/* character mask bitmap		*/
    register short *xpos;	/* x position of characters in mask	*/
    register FONT *font;	/* font					*/
    register FontPriv *fpriv;	/* font "private" parts			*/

#ifdef DEBUG
if (debug & D_Font)
    printf("GetFont(name=\"%s\")\n", name);
#endif DEBUG

    /* build pathname */
    strcpy(pathname, DEFAULT_FONT_DIRECTORY);
    strcat(pathname, name);
    strcat(pathname, DEFAULT_FONT_SUFFIX);

    /* open as "pathname", if open fails try "name" */
    if ((file = open(pathname, O_RDONLY, 0)) == -1 &&
	(errno != ENOENT || (file = open(name, O_RDONLY, 0)) == -1)) {
	errno = EINVAL;
	return (NULL);
    }

    /* read header and swap bytes in shorts */
    if (read(file, (caddr_t) &hdr, sizeof(FontData)) != sizeof(FontData)) {
	close(file);
	errno = EINVAL;
	return (NULL);
    }
    SwapShorts((short *) &hdr, sizeof(FontData));

    /* read font mask, and swap bits in bytes */
    mask_size = BitmapSize(chars->bm_width, chars->bm_height);
    mask = (char *) Xalloc(mask_size);
    lseek(file, (long) hdr.f_characters[0], 0);
    if (read(file, mask, mask_size) != mask_size) {
	close(file);
	free(mask);
	errno = EINVAL;
	return (NULL);
    }
    SwapBits((short *) mask, mask_size);

    /* read x position array */
    if (hdr.f_fixedWidth == 0) {
	xpos_size = (hdr.f_lastChar - hdr.f_firstChar + 2) * sizeof(short);
	xpos = (short *) Xalloc(xpos_size);
	lseek(file, (long) hdr.f_leftArray[0], 0);
	if (read(file, (caddr_t) xpos, xpos_size) != xpos_size) {
	    close(file);
	    free(mask);
	    free((caddr_t) xpos);
	    errno = EINVAL;
	    return (NULL);
	}
	SwapShorts(xpos, xpos_size);
    } else {
	xpos_size = 0;
	xpos = NULL;
    }

    close(file);

    /* complete "font" struct with info from file */
    font = (FONT *) Xalloc(sizeof (FONT));

    font->name	= (char *) Xalloc(strlen(name) + 1);
    strcpy(font->name, name);
    font->first	= hdr.f_firstChar;
    font->last	= hdr.f_lastChar;
    font->space	= hdr.f_spaceIndex;
    font->space	+= font->first;
    font->height = chars->bm_height;
    if (hdr.f_fixedWidth) {
	font->avg_width = hdr.f_fixedWidth;
	font->fixed = 1;
    } else {
	font->avg_width = (xpos[font->last] - xpos[font->first]) /
	    (font->last - font->first);
	font->fixed = 0;
    }
    font->base	= hdr.f_baseline;
    font->refcnt = 1;
    fpriv = (FontPriv *) Xalloc(sizeof(FontPriv));
    font->data	= (caddr_t) fpriv;

    /* complete "fpriv" struct */
    {
	BITMAP *bitmap = (BITMAP *) Xalloc(sizeof(BITMAP));
	RASTER *raster = (RASTER *) Xalloc(sizeof(RASTER));

	bitmap->width	= chars->bm_width;
	bitmap->height	= chars->bm_height;
	bitmap->refcnt	= 1;
	bitmap->kind	= (char) 0;
	bitmap->data	= (caddr_t) raster;

	raster->width	= (short) ((chars->bm_width+15)>>4)<<1;
	raster->address	= (short *) mask;

	fpriv->mask	= (BITMAP *) bitmap;
	fpriv->xpos	= xpos;

	/* if x position array exists use it to create width array */
	if (xpos) {
	    register short *p, *limitp;

	    fpriv->widths = (short *) Xalloc (xpos_size);

	    bcopy((char *)xpos, fpriv->widths, xpos_size);

	    limitp = &fpriv->widths[font->last];
	    for (p = &fpriv->widths[font->first]; p <= limitp; ++p) {
		*p = p[1] - *p;
	    }
	} else {
	    fpriv->widths = NULL;
	}
    }

    /* initialize font_pixmaps */
    {
	register int i;
	for (i=0; i<FONTPIXMAPS; ++i) {
	    fpriv->font_pixmaps[i].p = NULL;
	}
	fpriv->next_pixmap = 0;
    }

    return (font);
#undef chars
}

/*
 *	FreeFont
 */
FreeFont(font)
register FONT	*font;
{
    register FontPriv	*fpriv = FDATA(font);
    BITMAP		*bitmap = (BITMAP *)fpriv->mask;
    register RASTER	*raster = (RASTER *)bitmap->data;

#ifdef DEBUG
if (debug & D_Font)
    printf("FreeFont(font=0x%x)\n", font);
#endif DEBUG

    /* free text bitmap */
    free((caddr_t) raster->address);
    free((caddr_t) raster);
    free((caddr_t) bitmap);

    /* free xpos and widths arrays */
    if (fpriv->xpos) {
	free((caddr_t) fpriv->xpos);
	free((caddr_t) fpriv->widths);
    }

    /* free font pixmaps */
    {
	register int i;
	register struct _font_pixmaps *font_pixmaps = &fpriv->font_pixmaps[0];
	for (i = 0; i < FONTPIXMAPS; ++i, ++font_pixmaps) {
	    if (font_pixmaps->p) {
		FreePixmap(font_pixmaps->p);
	    }
	}
    }

    /* free remainder of font data */
    free((caddr_t) fpriv);
    free(font->name);
    free((caddr_t) font);
}

/*
 *	MakeFontPixmap
 */
PIXMAP *MakeFontPixmap(font, fore, back)
FONT *font;
register int	fore, back;
{
    register FontPriv *fdata = FDATA(font);
    register struct _font_pixmaps *font_pixmap = &fdata->font_pixmaps[0];
    register int i;

#ifdef DEBUG
if (debug & D_FontPixmap)
    printf("MakeFontPixmap(font=0x%x, fore=%d, back=%d)\n", font, fore, back);
#endif DEBUG

    /* search font_pixmaps for match */
    for (i = 0; i < FONTPIXMAPS; ++i) {
	if (font_pixmap->p &&
	    font_pixmap->fore == fore && font_pixmap->back == back) {
	    /* found match */
	    return (font_pixmap->p);
	}
    }

    /* free entry if necessary */
    font_pixmap = &fdata->font_pixmaps[fdata->next_pixmap];
    if (font_pixmap->p != NULL) {
	FreePixmap(font_pixmap->p);
    }

    /* create pixmap and save in font_pixmaps */
    font_pixmap->fore = fore;
    font_pixmap->back = back;
    font_pixmap->p = MakePixmap(fdata->mask, fore, back);
    fdata->next_pixmap = (fdata->next_pixmap + 1) % FONTPIXMAPS;

    return (font_pixmap->p);
}
