/* $XConsortium: XRdBitF.c,v 1.15 91/02/01 16:34:46 gildea Exp $ */
/* Copyright, 1987, Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/*
 *	Code to read bitmaps from disk files. Interprets 
 *	data from X10 and X11 bitmap files and creates
 *	Pixmap representations of files. Returns Pixmap
 *	ID and specifics about image.
 *
 *	Modified for speedup by Jim Becker, changed image
 *	data parsing logic (removed some fscanf()s). 
 *	Aug 5, 1988
 *
 * Note that this file and ../Xmu/RdBitF.c look very similar....  Keep them
 * that way (but don't use common source code so that people can have one 
 * without the other).
 */

#include "Xlibint.h"
#include <X11/Xos.h>
#include "Xutil.h"
#include <stdio.h>
#include <ctype.h>


#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];		/* conversion value */
static Bool initialized = False;	/* easier to fill in at run time */


/*
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 */
static void initHexTable()
{
    /*
     * We build the table at run time for several reasons:
     *
     *     1.  portable to non-ASCII machines.
     *     2.  still reentrant since we set the init flag after setting table.
     *     3.  easier to extend.
     *     4.  less prone to bugs.
     */
    hexTable['0'] = 0;	hexTable['1'] = 1;
    hexTable['2'] = 2;	hexTable['3'] = 3;
    hexTable['4'] = 4;	hexTable['5'] = 5;
    hexTable['6'] = 6;	hexTable['7'] = 7;
    hexTable['8'] = 8;	hexTable['9'] = 9;
    hexTable['A'] = 10;	hexTable['B'] = 11;
    hexTable['C'] = 12;	hexTable['D'] = 13;
    hexTable['E'] = 14;	hexTable['F'] = 15;
    hexTable['a'] = 10;	hexTable['b'] = 11;
    hexTable['c'] = 12;	hexTable['d'] = 13;
    hexTable['e'] = 14;	hexTable['f'] = 15;

    /* delimiters of significance are flagged w/ negative value */
    hexTable[' '] = -1;	hexTable[','] = -1;
    hexTable['}'] = -1;	hexTable['\n'] = -1;
    hexTable['\t'] = -1;
	
    initialized = True;
}

/*
 *	read next hex value in the input stream, return -1 if EOF
 */
static NextInt (fstream)
    FILE *fstream;
{
    int	ch;
    int	value = 0;
    int gotone = 0;
    int done = 0;
    
    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

    while (!done) {
	ch = getc(fstream);
	if (ch == EOF) {
	    value	= -1;
	    done++;
	} else {
	    /* trim high bits, check type and accumulate */
	    ch &= 0xff;
	    if (isascii(ch) && isxdigit(ch)) {
		value = (value << 4) + hexTable[ch];
		gotone++;
	    } else if ((hexTable[ch]) < 0 && gotone)
	      done++;
	}
    }
    return value;
}


#if NeedFunctionPrototypes
int XReadBitmapFile (
    Display *display,
    Drawable d,
    _Xconst char *filename,
    unsigned int *width,                /* RETURNED */
    unsigned int *height,               /* RETURNED */
    Pixmap *pixmap,                     /* RETURNED */
    int *x_hot,                         /* RETURNED */
    int *y_hot)                         /* RETURNED */
#else
int XReadBitmapFile (display, d, filename, width, height, pixmap, x_hot, y_hot)
    Display *display;
    Drawable d;
    char *filename;
    unsigned int *width, *height;       /* RETURNED */
    Pixmap *pixmap;                     /* RETURNED */
    int *x_hot, *y_hot;                 /* RETURNED */
#endif
{
    Pixmap pix;				/* value to return */
    FILE *fstream;			/* handle on file  */
    unsigned char *data = NULL;		/* working variable */
    char line[MAX_SIZE];		/* input line from file */
    int size;				/* number of bytes of data */
    char name_and_type[MAX_SIZE];	/* an input line */
    char *type;				/* for parsing */
    int value;				/* from an input line */
    int version10p;			/* boolean, old format */
    int padding;			/* to handle alignment */
    int bytes_per_line;			/* per scanline of data */
    unsigned int ww = 0;		/* width */
    unsigned int hh = 0;		/* height */
    int hx = -1;			/* x hotspot */
    int hy = -1;			/* y hotspot */

    /* first time initialization */
    if (initialized == False) initHexTable();

    if ((fstream = fopen(filename, "r")) == NULL) {
	return BitmapOpenFailed;
    }

    /* error cleanup and return macro	*/
#define	RETURN(code) { if (data) free (data); fclose (fstream); return code; }

    while (fgets(line, MAX_SIZE, fstream)) {
	if (strlen(line) == MAX_SIZE-1) {
	    RETURN (BitmapFileInvalid);
	}
	if (sscanf(line,"#define %s %d",name_and_type,&value) == 2) {
	    if (!(type = rindex(name_and_type, '_')))
	      type = name_and_type;
	    else
	      type++;

	    if (!strcmp("width", type))
	      ww = (unsigned int) value;
	    if (!strcmp("height", type))
	      hh = (unsigned int) value;
	    if (!strcmp("hot", type)) {
		if (type-- == name_and_type || type-- == name_and_type)
		  continue;
		if (!strcmp("x_hot", type))
		  hx = value;
		if (!strcmp("y_hot", type))
		  hy = value;
	    }
	    continue;
	}
    
	if (sscanf(line, "static short %s = {", name_and_type) == 1)
	  version10p = 1;
	else if (sscanf(line,"static unsigned char %s = {",name_and_type) == 1)
	  version10p = 0;
	else if (sscanf(line, "static char %s = {", name_and_type) == 1)
	  version10p = 0;
	else
	  continue;

	if (!(type = rindex(name_and_type, '_')))
	  type = name_and_type;
	else
	  type++;

	if (strcmp("bits[]", type))
	  continue;
    
	if (!ww || !hh)
	  RETURN (BitmapFileInvalid);

	if ((ww % 16) && ((ww % 16) < 9) && version10p)
	  padding = 1;
	else
	  padding = 0;

	bytes_per_line = (ww+7)/8 + padding;

	size = bytes_per_line * hh;
	data = (unsigned char *) Xmalloc ((unsigned int) size);
	if (!data) 
	  RETURN (BitmapNoMemory);

	if (version10p) {
	    unsigned char *ptr;
	    int bytes;

	    for (bytes=0, ptr=data; bytes<size; (bytes += 2)) {
		if ((value = NextInt(fstream)) < 0)
		  RETURN (BitmapFileInvalid);
		*(ptr++) = value;
		if (!padding || ((bytes+2) % bytes_per_line))
		  *(ptr++) = value >> 8;
	    }
	} else {
	    unsigned char *ptr;
	    int bytes;

	    for (bytes=0, ptr=data; bytes<size; bytes++, ptr++) {
		if ((value = NextInt(fstream)) < 0) 
		  RETURN (BitmapFileInvalid);
		*ptr=value;
	    }
	}
    }					/* end while */

    if (data == NULL) {
	RETURN (BitmapFileInvalid);
    }

    pix = XCreateBitmapFromData (display, d, (char *) data, ww, hh);
    if (pix == None) {
	RETURN (BitmapNoMemory);
    }
    *pixmap = pix;
    *width = ww;
    *height = hh;
    if (x_hot) *x_hot = hx;
    if (y_hot) *y_hot = hy;

    RETURN (BitmapSuccess);
}
