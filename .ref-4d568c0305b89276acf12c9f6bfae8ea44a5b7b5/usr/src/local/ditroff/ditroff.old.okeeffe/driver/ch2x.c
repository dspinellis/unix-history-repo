/*	ch2x.c	1.1	87/02/05
 *
 * Font translation to X format from character format.
 *
 *	Use:	ch2x  [ -i  -s ]  charfile  >  xfontfile
 *
 *		Takes input from charfile (which must be in the format written
 *	by one of the xxx2ch programs), converts to X format and writes it
 *	to stdout.  If charfile is missing, stdin is read.  The -i flag tells
 *	ch2x to ignore the character codes at the start of each glyph
 *	definition, and pack the glyphs in consecutive code positions starting
 *	with 0.  Unlike other conversion programs, white space around a glyph
 *	is preserved.  Therefore, the -s option is ignored.
 */

#include <stdio.h>
#include "xfont.h"


#define RES		75	/* for xfont, resolution is 75 */
#define MAXLINE		300
#define	GLYPHSPACE	(MAXLINE * MAXLINE / 8)
#define DIRSIZ		256	/* maximum # of entries */


char *	malloc();
char *	index();

struct FontData	FH;		/* font file header */
struct GlyphData {
	short up;
	short down;
	short left;
	short right;
	short nbytes;
	short width;
	char *ptr;
} g[DIRSIZ];			/* table of glyph definitions */
short	leftarea[DIRSIZ];
char 	bitmap[GLYPHSPACE];
char *	newbitmap;

int	code;
int	printwidth, width, length, refv, refh;
int	totalwidth, maxleft, maxup, maxdown;

int	ignorecode = 0;
FILE *	filep;
char	ibuff[MAXLINE];


main(argc,argv)
int argc;
char **argv;
{
    register int i;
    register int j;
    register int codeindex;
    register char *chp;
    register char *bitp;
    register int bit;
    float par;


    while (argc > 1 && argv[1][0] == '-') {
	switch(argv[1][1]) {
	case 'i':
		ignorecode = 1;
		break;

	case 's':
		break;
	default:
		error("%s, unknown option flag", argv[1]);
	}
	argc--; argv++;
    }

    if (argc == 2) {
	if ((filep = fopen (argv[1], "r")) == NULL)
	    error("can't open file \"%s\"", argv[1]);
    } else filep = stdin;

    codeindex = 0;
    totalwidth = 0;
    maxleft = 0;
    maxup = 0;
    maxdown = 0;
    FH.fixedWidth = -1;
    for (i = 0; i < DIRSIZ; g[i++].ptr = (char *) 0);

    if (fgets(ibuff, MAXLINE, filep) == NULL || strcmp(ibuff, "fontheader\n"))
	error("not a character font file");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == NULL)
	    error("input line too long");

	if (ibuff[0] == ':') {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header \"%s\"", ibuff);
	    if (ignorecode) codeindex++; else codeindex = code;
	    if (codeindex < 0 || codeindex >= DIRSIZ)
		error("code (%d) out of range", codeindex);
	    printwidth = par + 0.5;

	    chp = &ibuff[0];
	    bitp = &bitmap[-1];
	    if (fgets(chp, MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(chp) - 1;
	    refv = -1;

	    for (length = 0; *chp != '\n'; length++) {
		bit = 0x100;
		for (i = 0; i < width; i++, chp++) {
		    if (bit == 0x100) {
			if (++bitp >= &bitmap[GLYPHSPACE])
			    error ("out of glyph space");
			*bitp = 0;
			bit = 1;
		    }
		    switch (*chp) {
			case '.':
				break;
			case 'x':
				refh = i;
				refv = length;
				break;
			case 'X':
				refh = i;
				refv = length;
			case '@':
			case '*':
				*bitp |= bit;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		    bit <<= 1;
		} /* for i */
		chp = &ibuff[0];
		if (fgets(chp, MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    } /* for length */

	    if (refv < 0) error("no reference point in glyph %d.", code);

	    g[codeindex].up = refv;
	    g[codeindex].down = length - refv;
	    g[codeindex].right = width - refh;
	    if (g[codeindex].right > printwidth)
		printwidth = g[codeindex].right;
	    g[codeindex].left = refh;
	    g[codeindex].nbytes = bitp - bitmap + 1;
	    g[codeindex].width = printwidth;
	    totalwidth += printwidth;
	    if (FH.fixedWidth == -1)
		FH.fixedWidth = printwidth;
	    if (FH.fixedWidth != printwidth)
		FH.fixedWidth = 0;
	    if (g[codeindex].left > maxleft) maxleft = g[codeindex].left;
	    if (g[codeindex].up > maxup) maxup = g[codeindex].up;
	    if (g[codeindex].down > maxdown) maxdown = g[codeindex].down;

				/* copy the bits to private place */
	    if ((g[codeindex].ptr = malloc(g[codeindex].nbytes)) == NULL)
		error("out of memory reading in file");
	    bcopy(bitmap, g[codeindex].ptr, g[codeindex].nbytes);
	} /* if ibuff == : */
    } /* while not EOF */

    if (totalwidth == 0)
	error("empty font");

    /*
     * Fill in the "fontData" header for this font.  fixedWidth is already set.
     */
    FH.waste = 0;
    FH.bitsPerPixel = 1;
    FH.spaceIndex = 32;			/* we can only guess */
    for (i = 0; g[i].ptr == NULL; i++)
	;
    FH.firstChar = i;
    for (i = DIRSIZ - 1; g[i].ptr == NULL; i--)
	;
    FH.lastChar = i;
    FH.bmHeight = maxup + maxdown;
    FH.baseline = maxup + 1;
    FH.leftArray = sizeof (FH);
    FH.bitmapPtr = FH.leftArray + (FH.lastChar-FH.firstChar+2) * sizeof(short);

    /*
     * calculate "leftarea" - the pointers for each glyph into the bitmap
     */
    leftarea[FH.firstChar] = 0;
    for (i = FH.firstChar; i <= FH.lastChar; i++) {
	if (g[i].ptr == NULL) {
	    FH.fixedWidth = 0;
	    leftarea[i + 1] = leftarea[i];
	} else {
	    leftarea[i + 1] = leftarea[i] + g[i].width + maxleft;
	    totalwidth += maxleft;
	}
    }
    FH.bmWidth = totalwidth;
    width = ((totalwidth + 15) >> 3) & ~1;
    newbitmap = bitmap;
    if (width > GLYPHSPACE)
	if ((newbitmap = malloc(width)) == NULL)
	    error("out of memory writing file");

    vwrite(&FH, sizeof(FH));
    vwrite(&leftarea[FH.firstChar],(FH.lastChar-FH.firstChar+2)*sizeof(short));
    /*
     * Calculate and write out the "strike" bitmap
     */
    for (length = 0; length < FH.bmHeight; length++) {
	bitp = newbitmap;
	*bitp = 0;
	i = 1;
	for (codeindex = FH.firstChar; codeindex <= FH.lastChar; codeindex++) {
	    if (g[codeindex].ptr != NULL) {
		for (j = -maxleft; j < g[codeindex].width; j++) {
		    if (bitset(&g[codeindex], j, maxup)) {
			*bitp |= i;
		    }
		    i <<= 1;
		    if (i == 0x100) {
			*++bitp = 0;
			i = 1;
		    }
		}
	    }
	}
	vwrite(newbitmap, width);
	maxup--;
    }
    exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	vwrite (buffer, buffer_size)
 |
 | Results:	writes out character array "buffer" of size "buffer_size"
 |		in sizes that "write" can handle
 *----------------------------------------------------------------------------*/

vwrite(buf, bufsize)
char *buf;
int bufsize;
{
	int tsize = 0;

	while (bufsize) {
		buf += tsize;
		if ((tsize = write(1, buf, bufsize)) < 0) {
			perror("ch2x: write failed");
			exit(-1);
		}
		bufsize -= tsize;
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	bitset (GlyphData_pointer, x_position, y_position)
 |
 | Results:	Given a Glyph definition and an x,y position (relative to the
 |		reference point of the glyph) bitset returns non-zero if the
 |		glyph has a pixel set at that point.
 *----------------------------------------------------------------------------*/

/*VARARGS1*/
bitset(g, x, y)
register struct GlyphData *g;
register int x;
register int y;
{
	register char *p;

	x += g->left;
	y = g->up - y;
	if (x < 0 || y < 0 || x >= (g->left+g->right) || y >= (g->up+g->down))
		return(0);
	p = g->ptr + (x >> 3) + y * ((g->left + g->right + 7) >> 3);
	return( *p & (1 << (x&7)) );
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
	fprintf(stderr, "ch2x: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}
