/*	x2ch.c	1.1	87/02/05
 *
 * Font translation for X font-style fonts to character format.
 *
 *	Use:  x2ch  [ -p# ] fontfile  [ character_list ]
 *
 *		Reads "fontfile" from current directory (or if not found,
 *	from BITDIR defined below) and converts it to a character font format
 *	editable by real people, and convertable BACK to X format by the
 *	ch2x program.  The -p option specifies the "point size" of the font.
 *	If no "-p#" is specified, the pointsize is 10.  Output goes to stdout.
 */
/* #define DEBUG   	/* if defined, statistics are printed to stderr */

#include <stdio.h>
#include "xfont.h"


#define	DIRSIZ	256
#define BITDIR		"/usr/new/lib/X/font"


struct FontData FH;
#define	first	FH.firstChar
#define	last	FH.lastChar
short	bitmapindex[DIRSIZ + 1];/* bit offsets in bitmap */
short	widths[DIRSIZ + 1];	/* widths calculated from but offsets */
char	*charbits;		/* pointer to start of bitmap */
char	**lineptr;		/* pointers to start of each line of bitmap */
int	pointsize = 10;		/* can only be changed from command line */
int	tmp;			/* used for various short-lived things */
int	*bitcount;		/* place to hold count of bits on a particular
				   line in bitmap - used to find the baseline */

char	IName[100];			/* input file name building place */
unsigned char	defascii[DIRSIZ];	/* default characters to print */
unsigned char	*charswanted = defascii;


main(argc, argv)
int argc;
char **argv;
{
	register int i;
	register int j;
	int FID;

	if (argc < 2 || argc > 4) {
    usage:
		error("usage: %s [ -p# ] filename [ character-list ]", argv[0]);
	}
	if (argv[1][0] == '-') {
		argv++;
		if (argv[0][1] != 'p')
			goto usage;
		pointsize = atoi(&(argv[0][2]));
	}
	for (i = 0; i < DIRSIZ; i++) {
		bitmapindex[i] = 0;
		widths[i] = 0;
		defascii[i] = i;
	}
	if (argc == 3)
		charswanted = (unsigned char *) argv[2];

	/*
	 * find font in BITDIR or current directory, also trying to
	 * tack on the ".onx" extension onto the filename
	 */
	++argv;
	sprintf(IName, "%s/%s.onx", BITDIR, *argv);
	if ((FID = open(*argv, 0)) < 0) {
		if ((FID = open(IName, 0)) < 0) {
			sprintf(IName, "%s.onx", *argv);
			if ((FID = open(IName, 0)) < 0) {
				sprintf(IName, "%s/%s", BITDIR, *argv);
				if ((FID = open(IName, 0)) < 0)
					error("Can't find %s", *argv);
			}
		}
	}

	if (read(FID, &FH, sizeof FH) != sizeof FH)
		error("no header in Font file.");
	if (first < 0 || last >= DIRSIZ || last <= first)
		error("font boundaries (%d,%d) out of range", first, last);
	if (FH.bmWidth <= 0 || FH.bmWidth >= 20000 ||
			FH.bmHeight <= 0 || FH.bmHeight >= 20000)
		error("dimensions (%d,%d) out of range",FH.bmWidth,FH.bmHeight);
	i = (((FH.bmWidth + 15) >> 3) &~ 1) * FH.bmHeight;
	charbits = (char *) malloc(i);

#ifdef DEBUG
	fprintf(stderr,"Bit Map Ptr = %d\nBitMap Width = %d\nBit Map Height = %d\nBitsPerPixel = %d\nFirst Character = %d\nLast Character = %d\nLeft Array = %d\nBaseLine = %d\nSpace Index = %d\nFixed Width = %d\nbit map size = %d\n", FH.bitmapPtr, FH.bmWidth, FH.bmHeight, FH.bitsPerPixel, FH.firstChar, FH.lastChar, FH.leftArray, FH.baseline, FH.spaceIndex, FH.fixedWidth, i);
#endif

	lseek (FID, (long) FH.bitmapPtr, 0);
	if (read(FID, charbits, i) != i)
		error("bit map (%d chars) not in Font file", i);

	if (FH.fixedWidth == 0) {
		i = (last - first + 2) * sizeof (short);
#ifdef DEBUG
		fprintf(stderr, "width array size = %d\n", i);
#endif
		lseek (FID, (long) FH.leftArray, 0);
		if (read(FID, &bitmapindex[first], i) != i)
			error("width map not in Font file");
	} else {
		for (i = first + 1; i <= last + 1; i++)
			bitmapindex[i] = bitmapindex[i - 1] + FH.fixedWidth;
	}
	/*
	 * figure out character widths from "leftarray"
	 */
#ifdef DEBUG
	fprintf(stderr,"left,widths:\n");
#endif
	for (i = first; i <= last; i++) {
		if ((widths[i] = bitmapindex[i + 1] - bitmapindex[i]) < 0)
			error ("inconsistent width table");
#ifdef DEBUG
		fprintf(stderr,"%03d:%5d,%6d\n", i, bitmapindex[i], widths[i]);
#endif
	}
	lineptr = (char **) malloc((FH.bmHeight + 2) * sizeof (char *));
	j = ((FH.bmWidth + 15) >> 3) &~ 1;
	lineptr[0] = charbits;
	for (i = 1; i <= FH.bmHeight; i++) {
		lineptr[i] = lineptr[i - 1] + j;
	}

	/*
	 * If not given a baseline, try to figure one out by counting the
	 * bits in a given row.  When the number falls suddenly, that's the
	 * baseline.  This is not guaranteed to work.
	 */
	if (--FH.baseline < 0) {
		tmp = 0;
		bitcount = (int *) malloc((FH.bmHeight + 2) * sizeof (int));
		for (i = 0; i < FH.bmHeight; i++) {
			bitcount[i] = 0;
			for (j = 0; j < FH.bmWidth; j++) {
				if (bitset(lineptr[i], j))
					bitcount[i]++;
			}
			tmp += bitcount[i];
#ifdef DEBUG
			fprintf(stderr, "bitcount[%d] == %d\n", i, bitcount[i]);
#endif
		}
		tmp /= FH.bmHeight + 1;
#ifdef DEBUG
		fprintf(stderr, "average == %d\n", tmp);
#endif
		for (i = 1; i < FH.bmHeight; i++) {
			if (bitcount[i-1] > tmp && bitcount[i] < (tmp >> 1))
				FH.baseline = i - 1;
		}
		if (FH.baseline < 0)
			FH.baseline = 0;
	}

	printf("fontheader\ndesiz %d\nmag 1000\n", pointsize);
	printf("rot 0\ncadv 0\nladv 1\nid 0\nres 75\n");

	for (i = 0; i < DIRSIZ; i++) {
		j = charswanted[i];
		if (i > 0 && j == 0)
			break;
		if (widths[j]) {
			register int k;
			register int l;

			printf(":%d, width = %d.00\n", j, widths[j]);

			for (k = 0; k < FH.bmHeight; k++) {
			    for (l = 0; l < widths[j]; l++) {
				if (bitset(lineptr[k], bitmapindex[j]+l)) {
					if (l == 0 && k == FH.baseline)
						putchar('X');
					else
						putchar('@');
				} else {
					if (l == 0 && k == FH.baseline)
						putchar('x');
					else
						putchar('.');
				}
			    }
			    putchar('\n');
			}

			putchar('\n');
		}
	}
	exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	error (format_string, argument1, argument2.... )
 |
 | Results:	fprints a message to standard error, then exits with error
 |		code 1
 |
 | Side Efct:	This routine does NOT return
 *----------------------------------------------------------------------------*/

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "x2ch: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}


/*----------------------------------------------------------------------------*
 | Routine:	bitset (bitstream pointer, bit number)
 |
 | Results:	returns nonzero if a bit is set in a supplied bit stream.
 |		No range checking is done on anything.  Bit order is least
 |		significant to most significant.
 *----------------------------------------------------------------------------*/

bitset(stream, bit)
char *stream;
register int bit;
{
	register char *p;

	p = stream + (bit >> 3);
	return ((*p) & (1 << (bit & 7)));
}
