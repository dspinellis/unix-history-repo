/*
	dev.h: characteristics of a typesetter
*/

struct dev {
	unsigned short	filesize;	/* number of bytes in file, */
				/* excluding dev part */
	unsigned short	res;		/* basic resolution in goobies/inch */
	unsigned short	hor;		/* goobies horizontally */
	unsigned short	vert;
	unsigned short	unitwidth;	/* size at which widths are given, in effect */
	unsigned short	nfonts;		/* number of fonts physically available */
	unsigned short	nsizes;		/* number of sizes it has */
	unsigned short	sizescale;	/* scaling for fractional point sizes */
	unsigned short	paperwidth;	/* max line length in units */
	unsigned short	paperlength;	/* max paper length in units */
	unsigned short	nchtab;		/* number of funny names in chtab */
	unsigned short	lchname;	/* length of chname table */
	unsigned short	spare1;		/* #chars in largest ever font */
	unsigned short	spare2;		/* in case of expansion */
};

struct Font {		/* characteristics of a font */
	unsigned char	nwfont;		/* number of width entries for this font */
	unsigned char	specfont;	/* 1 == special font */
	unsigned char	ligfont;	/* 1 == ligatures exist on this font */
	unsigned char	spare1;		/* unused for now */
#ifdef 0
	unsigned char	fonttab;	/* 1 == use extra table for fontnumbers */
	unsigned char	slant;		/* if set, slant font by slant degrees */
#endif
	unsigned char	namefont[10];	/* name of this font (e.g., "R" */
	unsigned char	intname[10];	/* internal name (=number) on device, in ascii */
};

/* ligatures, ORed into ligfont */

#define	LFF	01
#define	LFI	02
#define	LFL	04
#define	LFFI	010
#define	LFFL	020

/*
 * Notes by jaap:
 *
 * spare1 int struct dev is also known as biggestfont
 *
 * in Font structure is added:
 *	fonttab: if set to 1, the Font.out has an extra
 *		  table of shorts which gives the physical font
 *		  on which the chracter lives. Allows mapping of
 *		  "logial fonts" into variuos physical fonts on the
 *		  device. Needed since the Harris f.i. has a weird font
 *		  lay-out. Also makes fonts consisting of weird
 *		  character combinations easier.
 *	slant:	The font can must be slanted to force italics (function
 *		of back-end, necessary for f.i. the Harris, which
 *		doesn't has italics for the sans-serif fonts; these
 *		italics have to be made by slanting)
 */
