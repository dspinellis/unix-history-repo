/* font.c	Reads a font from a file and stores it on the workstation
 *
 *	GetFont		Takes a font name and stores it
 *	FreeFont	Frees the storage taken by a font
 *
 * Modification History
 *
 *	Carver 8601.13 Fix reference to ../libvs100/param.h to be param.h
 *
 *      Jones  8510.15 Fix ``memory leak'' -- deallocate leftarry in FreeFont
 *
 * 	Carver 8510.03 Increased the allocation size of the left array buffer
 *		       by 1 word.  Fixes boundary problem.
 */

#include "ddxqvss.h"
#include "param.h"
#include <errno.h>

extern int errno;
extern char *ddxfontdir;
extern char *ddxfontsuffix;
char *Xalloc(), *strcpy(), *strcat();
long lseek();

#define CHARPERFONT 256

FONT *GetFont (name)
	char *name;
{
	char fontname[256];
	int fontfile;
	FontData font;
#define chars ((BitMap *) font.f_characters)
	int fontsize, leftsize, width, i, j;
	char *fontarea;
	register short *leftarea, *leftarray;
	register FONT *fd;
	register FontPriv *fpriv;
	int tablesize = (CHARPERFONT + 1) * sizeof(short);  /* 8510.03 Carver */

	strcpy (fontname, ddxfontdir);
	strcat (fontname, name);
	strcat (fontname, ddxfontsuffix);

	if ((fontfile = open (fontname, 0)) == -1 &&
	    (errno != ENOENT || (fontfile = open (name, 0)) == -1)) {
	    errno = EINVAL;
	    return (NULL);
	}

	if (read (fontfile, (caddr_t) &font, sizeof (FontData)) != sizeof (FontData)) {
	    close (fontfile);
	    errno = EINVAL;
	    return (NULL);
	}

	fontsize = BitmapSize(chars->bm_width, chars->bm_height);
	fontarea = (char *) Xalloc (fontsize);
	lseek (fontfile, (long) font.f_characters[0], 0);
	if (read (fontfile, fontarea, fontsize) != fontsize) {
	    close (fontfile);
	    free (fontarea);
	    errno = EINVAL;
	    return (NULL);
	}

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
		DeviceError ("Bad font leftarray!\n");
		}
	    fpriv->widths[i - fd->first] = width;
	}
		
        fd->avg_width = ((fpriv->leftarray[fd->last + 1]  - 
		fpriv->leftarray[fd->first]) / (fd->last - fd->first + 1));

/*	striketobitmaps(fd);*/
	return (fd);
#undef chars
}

FreeFont (font)
	register FONT *font;
{
	register FontPriv *data;

	data = FDATA(font);
#ifdef DoStrikeArray
	if (data->chrs) free ((caddr_t) data->chrs);
#endif
	if (data->leftarray) free ((caddr_t) data->leftarray);
	if (data->widths) free ((caddr_t) data->widths);
	free (data->fltable);
	FreeBitmap(data->strike);
	free ((caddr_t) data);
	free (font->name);
	free ((caddr_t) font);
}

/* 
 * this routine converts strike format into an array of bitmaps 
 */
#ifdef DoStrikeArray
striketobitmaps(fn)
	FONT *fn;
{
	register FontPriv *fp = FDATA(fn);
	int fheight = fn->height;
	int i, j;
	register long *bits;
	register int tmp;
	int length = 
		(fn->last - fn->first + 1) * fheight * sizeof(long);
	if (fp->maxwidth > 32) return;
	fp->chrs = bits = (long *)Xalloc( length );
	bzero(bits, length);

	for (i = fn->first; i <= fn->last; i++) {
	    register w = fp->widths[i];
	    register offset = fp->leftarray[i];
	    if (w < 0) w = 0;		/* sanity check for bad fonts */
		for (j = 0; j < fheight; j++) {
			register char *base = fp->fltable[j];
			tmp = extzv(base, offset, w);
		    *bits = tmp;
		    bits += 1;
		}
	}
	return;
}
#endif
