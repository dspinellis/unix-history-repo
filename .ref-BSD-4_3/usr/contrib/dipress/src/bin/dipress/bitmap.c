/*
 *  bitmap - routines that help build a bitmap of a graphical image
 *
 *  These are primarily used to implement all of the drawing primitives
 *  (except for lines)
 *
 *  History:
 *28-aug-85 John Mellor-Crummey	added flush_bitmap which is used instead
 *				of print_bitmap if current page is not outputted
 *
 *	17-jun-85  ed flint	bitmap width(bm_width) must be multiple of 32
 *				bits for print services 12 (?)
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 */

# include <stdio.h>
# include "iptokens.h"
# include "literal.h"
# include "operator.h"

# define    local	static
# define    No		0
# define    Yes		1

# define    INCH	2540		/* micas per inch */

extern int drawidth;
extern double drawscale;

char *malloc();

local int minx;
local int miny;
local int maxx;
local int maxy;
local int bm_width;		/* in real bytes */
local int bm_size;
local char *bitmap;
local char *bm_prelude;
local char  bm_active = No;

# define    scale_x(x)		x = (double)x * drawscale
# define    scale_y(y)		y = (double)y * drawscale

/*
 *  new_bitmap(x1, y1, x2, y2) - prepare for a new bitmap, the extreme x and y
 *				 values are x1, y1, x2, and y2.
 */

new_bitmap(stroke, x1, y1, x2, y2)

int stroke, x1, y1, x2, y2;

{
    int new_size;
    register int temp;

#ifdef DEBUG
    fprintf(stderr, "new_bitmap(%d, %d, %d, %d, %d)\n", stroke,x1, y1, x2, y2);
#endif
    drawidth = stroke * drawscale;
    scale_x(x1);
    scale_y(y1);
    scale_x(x2);
    scale_y(y2);
#ifdef DEBUG
    fprintf(stderr, "after scaling: width %d: %d, %d and %d, %d\n", drawidth, x1, y1, x2, y2);
#endif

    /* insure that x1, y1 is the lower left */
    if (x1 > x2)
    {
#ifdef DEBUG
	fprintf(stderr, "exchanging pairs ... ");
#endif
	temp = x1;	/* exchange x1 and x2 */
	x1   = x2;
	x2   = temp;
	temp = y1;	/* exchange y1 and y2 */
	y1   = y2;
	y2   = temp;
#ifdef DEBUG
	fprintf(stderr, "now %d, %d and %d, %d\n", x1, y1, x2, y2);
#endif
    }

    /* adjust the extremes to allow for pen thickness */
    temp = (drawidth + 1) / 2;
    x1 -= temp;
    y1 -= temp;
    x2 += temp;
    y2 += temp;

    if (!bm_active || !(x1 >= minx && y1 >= miny && x2 <= maxx && y2 <= maxy))
    {
	/* we need to set up a new map */
	/* but first, print the old one if it is still active */
#ifdef DEBUG
	fprintf(stderr, "setting up new map, bm_active is %d\n", bm_active);
#endif
	print_bitmap();

	minx = x1;
	miny = y1;
	maxx = x2;
	maxy = y2;

	bm_width = (y2 - y1 + 7) / 8;

	/* print services 12 (?) wants packed pixel vectors to produce each */
	/* scan line as a mutliple of 32 bits, this is backward compatible  */
	/* with previous versions of services				    */
	/* (the previous version of bitmap produced 16 bit multiples which  */
	/* will NOT work with services 12 (?) and beyond)		    */

	if ( bm_width%4 != 0 )
	{
	    bm_width= bm_width + (4 - bm_width%4);
	}
	new_size = (x2 - x1) * bm_width;
	if (new_size > bm_size)
	{
	    /* need to allocate a larger area for the bitmap */
	    if (bitmap != NULL)
	    {
		free(bitmap);
	    }
	    /* leave space for the ppv prelude (leading 4 bytes) */
	    bm_prelude = malloc((unsigned)(new_size + 4));
	    bitmap = bm_prelude + 4;
	    bm_size = new_size;
	}
#ifdef DEBUG
	fprintf(stderr, "bm_width %d, bm_size %d\n", bm_width, bm_size);
#endif
    
	bzero(bitmap, bm_size);
    }
}

set_pixel(x, y)

int x;
int y;

{
    int mask;
    int half_drawidth;
    register int i;
    register int ybit, temp;
    register char *ptr;

    scale_x(x);
    scale_y(y);

    bm_active = Yes;

#ifdef DEBUG
    if (x < minx || x > maxx || y < miny || y > maxy)
    {
	fprintf(stderr, "point off map: (%d, %d)\n", x, y);
	return;
    }
#endif
    x -= minx;
    y  = maxy - y;	/* yes, it works */
    half_drawidth = drawidth >> 1;

    /* Remember the bitmap is built in increasing y */

    /* draw the "x" line vertically */
    mask = 0x80 >> (y & 007);
    ptr = (x - half_drawidth) * bm_width + (y >> 3) + bitmap;
    for (i = 0; i < drawidth; i++, ptr += bm_width)
    {
	*ptr |= mask;
    }

    /* draw the "y" line horizontally */
    y -= half_drawidth;
    ptr = (x * bm_width) + (y >> 3) + bitmap;
    ybit = y & 007;
    temp = ybit + drawidth;
    if (temp < 8)
    {
	/* special case -- less than one byte */
	*ptr |= (0xff >> ybit) ^ (0xff >> temp);
    }
    else
    {
	*ptr |= (0xff >> ybit);
	for (i = drawidth - 8 + ybit; i > 8; i -= 8)
	{
	    *++ptr |= 0xff;
	}
	*++ptr |= ~(0xff >> i);
    }
}

flush_bitmap()
{
	bm_active = No;
}

print_bitmap()

{
    register int bits;
    register char *prelude = bm_prelude;
    int totalbits, size, bm_xdelta, bm_x = minx;

#ifdef DEBUG2
    int x;
    int y;
    int i;
    char *ptr;
#endif

    if (!bm_active)
    {
	/* don't bother */
	return;
    }

#ifdef DEBUG2
    /* debugging version for now */
    ptr = bitmap;
    for (x = minx; x <= maxx; x++)
    {
	for (y = miny; y <= maxy; y += 8, ptr++)
	{
	    for (i = 0x80; i != 0; i >>= 1)
	    {
		if (*ptr & i)
		{
		    putchar('*');
		}
		else
		{
		    putchar(' ');
		}
	    }
	}
	putchar('\n');
    }
#endif

/* The following is conservative (might actually be twice as big) */
#define MAXPPVSAMPS 262144
    bm_xdelta = maxx - minx;
    bits = bm_width << 3;			/* (* 8) */
    totalbits = bits * bm_xdelta;
    if (totalbits > MAXPPVSAMPS) {
	bm_xdelta = MAXPPVSAMPS / bits;
    }
    while (bm_x < maxx) {
	/* Check for last time through loop */
	if (bm_xdelta > (maxx - bm_x)) {
	    bm_xdelta = maxx - bm_x;
	}
	size = (bits * bm_xdelta) >> 3;		/* Could hand optimize! */
	/* inside a dosavesimplebody to preserve transform */
	Op(dosavesimplebody);
	Op(beginBody);
	AppendInteger((long) bm_xdelta);	/* xPixels */
	AppendInteger((long) bits);		/* yPixels */
	AppendInteger(1L);			/* samplesPerPixel */
	AppendInteger(1L);			/* maxSampleValue */
	AppendInteger(1L);			/* samplesInterleaved */
	Translate(0.0, 0.0);			/* (null) transform */

	/* spew out the packed pixel vector */
	prelude[0] = 0;
	prelude[1] = 1;
	prelude[2] = (char)((bits & 0xff00) >> 8);	/* high order byte */
	prelude[3] = (char) (bits & 0x00ff);		/* low  order byte */
	append_Sequence(sequencePackedPixelVector, size + 4,
						(unsigned char *) prelude);

	/* make the pixel array */
	Op(makepixelarray);

	/* set the transform to the right scale and position */
	/* transform must scale back to micas and position bitmap */
	AppendInteger((long) bm_x);
	AppendInteger((long) ((11 * INCH * drawscale) - maxy));
	Op(translate);
	AppendInteger(5L);
	Op(scale);
	Op(concat);
	Op(concatt);

	/* mask it and finish the simple-body */
	Op(maskpixel);
	Op(endBody);
	prelude += size;
	bm_x += bm_xdelta;
    }

    /* no longer active, now is it? */
    bm_active = No;
}
