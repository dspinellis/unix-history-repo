/* $Header: font.c,v 10.3 86/02/01 15:46:53 tony Rel $ */
/* font.c	Reads a font from a file and stores it on the workstation
 *
 *	GetFont		Takes a font name and stores it
 *	FreeFont	Frees the storage taken by a font
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
#include "vssite.h"
#include <errno.h>

extern int errno;

char *Xalloc(), *AllocateSpace(), *strcpy(), *strcat();
long lseek();
VSArea *VSAlloc();

FONT *GetFont (name)
	char *name;
{
	char fontname[256];
	int fontfile;
	FontData font;
	register FontData *fontCopy;
#define chars ((BitMap *) font.f_characters)
	int fontsize, leftsize, width;
	char *fontarea;
	register short *leftarea;
	register FONT *fd;
	FontPriv *fpriv;
	register BitMap *mychars;

	strcpy (fontname, DEFAULT_FONT_DIRECTORY);
	strcat (fontname, name);
	strcat (fontname, DEFAULT_FONT_SUFFIX);

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

	if (font.f_fixedWidth == 0) {
	    leftsize = (font.f_lastChar - font.f_firstChar + 2) * sizeof (short);
	    leftarea = (short *) Xalloc (leftsize);
	    lseek (fontfile, (long) font.f_leftArray[0], 0);
	    if (read (fontfile, (caddr_t) leftarea, leftsize) != leftsize) {
		close (fontfile);
		free (fontarea);
		free ((caddr_t) leftarea);
		errno = EINVAL;
		return (NULL);
	    }
	} else {
	    leftsize = 0;
	    leftarea = NULL;
	}

	close (fontfile);

	if ((fontCopy = (FontData *)
				AllocateSpace (sizeof (FontData))) == NULL) {
	    free (fontarea);
	    if (leftarea)
		free ((caddr_t) leftarea);
	    errno = ENOMEM;
	    return (NULL);
	}
	mychars = (BitMap *) fontCopy->f_characters;
	fd = (FONT *) Xalloc (sizeof (FONT));

	mychars->bm_address[0] = sizeof (FontData);
	mychars->bm_address[1] = 0;
	mychars->bm_width = chars->bm_width;
	fd->height = mychars->bm_height = chars->bm_height;
	mychars->bm_bitsPerPixel = 1;
	fd->first = fontCopy->f_firstChar = font.f_firstChar;
	fd->last = fontCopy->f_lastChar = font.f_lastChar;
	if (leftarea)
	    fontCopy->f_leftArray[0] = sizeof (FontData) + fontsize;
	else
	    fontCopy->f_leftArray[0] = 0;
	fontCopy->f_leftArray[1] = 0;

	fd->base = fontCopy->f_baseline = font.f_baseline;
	fd->space = fontCopy->f_spaceIndex = font.f_spaceIndex;
	fd->space += fd->first;
	if (fd->avg_width = fontCopy->f_fixedWidth = font.f_fixedWidth)
	    fd->fixed = 1;
	else
	    fd->fixed = 0;

	fd->refcnt = 1;
	fpriv = (FontPriv *) Xalloc (sizeof (FontPriv));
	fd->data = (caddr_t) fpriv;
	fpriv->widths = leftarea;

	if ((fpriv->remote = (VSArea *) VSAlloc (sizeof (FontData) + fontsize + leftsize,
						    FONT_TYPE)) == NULL) {
	    free (fontarea);
	    if (leftarea)
		free ((caddr_t) leftarea);
	    free ((caddr_t) fd);
	    free ((caddr_t) fpriv);
	    return (NULL);
	}
	fd->name = (char *) Xalloc (strlen (name) + 1);
	strcpy (fd->name, name);

	if (MoveObjectDown ((caddr_t) fontCopy, fpriv->remote->vsPtr, sizeof (FontData)) ||
	    MoveBufferDown (fontarea,
			    fpriv->remote->vsPtr + sizeof (FontData),
			    fontsize) ||
	    (leftarea &&
	     MoveBufferDown ((caddr_t) leftarea,
			     fpriv->remote->vsPtr + sizeof (FontData) + fontsize,
			     leftsize))) {
	    free (fontarea);
	    FreeFont (fd);
	    errno = ENOMEM;
	    return (NULL);
	}

	if (leftarea) {
	    leftsize = font.f_lastChar - font.f_firstChar + 1;
	    width = 0;
	    fontsize = 0;
	    while (--leftsize >= 0) {
		*leftarea = leftarea[1] - *leftarea;
		if (*leftarea) {
		    width += *leftarea;
		    fontsize++;
		}
		leftarea++;
	    }
	    fd->avg_width = width / fontsize;
	}
	free (fontarea);
	return (fd);
#undef chars
}

FreeFont (font)
	register FONT *font;
{
	register FontPriv *data;

	data = FDATA(font);
	if (data->widths) free ((caddr_t) data->widths);
	VSFree (data->remote);
	free ((caddr_t) data);
	free (font->name);
	free ((caddr_t) font);
}
