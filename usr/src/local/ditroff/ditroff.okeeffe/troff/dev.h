/*
	dev.h: characteristics of a typesetter
*/

struct dev {
	unsigned short	filesize;	/* number of bytes in file, */
				/* excluding dev part */
	short	res;		/* basic resolution in goobies/inch */
	short	hor;		/* goobies horizontally */
	short	vert;
	short	unitwidth;	/* size at which widths are given, in effect */
	short	nfonts;		/* number of fonts physically available */
	short	nsizes;		/* number of sizes it has */
	short	sizescale;	/* scaling for fractional point sizes */
	short	paperwidth;	/* max line length in units */
	short	paperlength;	/* max paper length in units */
	short	nchtab;		/* number of funny names in chtab */
	short	lchname;	/* length of chname table */
	short	spare1;		/* #chars in largest ever font */
	short	spare2;		/* in case of expansion */
};

struct Font {		/* characteristics of a font */
	char	nwfont;		/* number of width entries for this font */
	char	specfont;	/* 1 == special font */
	char	ligfont;	/* 1 == ligatures exist on this font */
	char	spare1;		/* unused for now */
	char	fonttab;	/* 1 == use extra table for fontnumbers */
	char	slant;		/* if set, slant font by slant degrees */
	char	namefont[10];	/* name of this font (e.g., "R" */
	char	intname[10];	/* internal name (=number) on device, in ascii */
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
 * in Font struvture is added:
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
