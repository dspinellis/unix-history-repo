/*
 *	Written by Eric C. Cooper, CMU
 *
 */
/* $Header: pxl.h,v 10.5 86/02/01 15:45:03 tony Rel $ */

#define BITS_PER_LONG 32
#define BITS_PER_SHORT 16
#define BITS_PER_BYTE 8

#define BYTES_PER_LONG (BITS_PER_LONG/BITS_PER_BYTE)
#define BYTES_PER_SHORT (BITS_PER_SHORT/BITS_PER_BYTE)

#define SHORTS_PER_LONG (BITS_PER_LONG/BITS_PER_SHORT)

#define ROUNDUP(x,y) (((x)+(y)-1)/(y))

/*
 * Raster ops.
 */
#define ROP_CLEAR	0
#define ROP_ERASE	2
#define ROP_COPYREV	3
#define ROP_COPY	12
#define ROP_PAINT	14
#define ROP_FILL	15

/*
 * Bitmap structure for raster ops.
 */	
struct bitmap{
	short h, w;		/* height and width in pixels */
	short bytes_wide;	/* scan-line width in bytes */
	char *bits;		/* pointer to the bits */
};

#define MAXCHARS 128		/* make 256 for 8 bit characters */

/*
 * Per-character information.
 * There is one of these for each character in a font.
 * All fields are filled in at font definition time,
 * except for the bitmap, which is "faulted in"
 * when the character is first referenced.
 */
struct glyph {
	long addr;		/* address of bitmap in PXL file */
	long dvi_adv;		/* DVI units to move reference point */
	short x, y;		/* x and y offset in pixels */
	struct bitmap bitmap;	/* bitmap for character */
	short pxl_adv;		/* pixels to move reference point */
};

/*
 * The layout of a font information block.
 * There is one of these for every loaded font or
 * magnification thereof.
 *
 * Also note the strange units.  The design size is in 1/2^20 point
 * units (also called micro-points), and the individual character widths
 * are in the TFM file in 1/2^20 ems units, i.e. relative to the design size.
 */

struct font {
	struct font *next;		/* link to next font info block */
	struct font *prev;		/* link to previous font info block */
	int TeXnumber;			/* font number (in DVI file) */
	int scale;			/* scaled size in DVI units */
	int design;			/* design size in DVI units */
	char *fontname;			/* PXL file name */
	FILE *file;			/* open PXL file or NULL */
	struct glyph glyph[MAXCHARS];
};
