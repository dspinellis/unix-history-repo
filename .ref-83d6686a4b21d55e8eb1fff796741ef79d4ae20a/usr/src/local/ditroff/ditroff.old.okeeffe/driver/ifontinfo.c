static char sccsid[] = "ifontinfo.c	1.7	(Berkeley)	86/03/04";

/* Font Information for Imagen-style fonts (RST format)
 *      taken from vfontinfo, by Andy Hertzfeld  4/79
 */

#include <stdio.h>
#include <ctype.h>
#include "rst.h"

#ifndef BITDIR
#define  BITDIR	"/usr/src/local/imagen/fonts/raster"
#endif

char *	rdchar();
char *	malloc();

char *idstrings;		/* place for identifying strings */
char *endstring;		/* points to the end of the id strings */
double	fixtowdth;		/* "fix" and magnification conversion factor */
glyph_dir g[DIRSIZ];		/* directory of glyph definitions */
preamble p;			/* set of variables for preamble */

char	*fontdir = BITDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
char	*rdchar ();		/* function makes strings for ascii */
FILE *	FID;			/* input file number */

char	defascii[DIRSIZ];	/* list of ascii characters - in order */
char	*charswanted = defascii;/* list of characters to print info for */
int	verbose = 0;		/* flag - whether to actually show chars */
char	charbits[10000];	/* place to store bits for a glyph */
int	gbase;			/* base address of glyphs in RST file */
int	H, W, WB, base;
int 	zoom = 1;

char	msgout[24][80];		/* place to store glyphs to print later */
int	msgflag = 0;		/* flag - use msgout and print later? */
int	curline, curcol;	/* cursor, numbered from lower left corner */
int	minline = 24;
int	maxline = 0;
int	maxcol = 0;


main(argc,argv)
int argc;
char **argv;
{
	register int i;
	register int j;

	while (argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'z':
			zoom = argv[1][2] - '0';  /* zoom implies verbose */
		case 'v':
			verbose++;
			break;
		case 'm':
			msgflag = 1;
			zoom = 2;
			for (i=0; i<24; i++)
				for (j=0; j<80; j++)
					msgout[i][j] = ' ';
			curline = 5; curcol = 0;
			break;
		default:
			error("bad flag: %s", argv[1]);
		}
		argc--; argv++;
	}
	if (argc < 2)
		error("usage: %s filename", argv[0]);

	for (i=0; i<DIRSIZ; i++)
		defascii[i] = i;
	if (argc >= 3)
		charswanted = argv[2];

	sprintf(IName, "%s/%s", fontdir, argv[1]);
	if ((FID = fopen(argv[1], "r")) == NULL)
		if ((FID = fopen(IName, "r")) == NULL)
			error("can't find %s",argv[1]);

	for (i = 0; i < FMARK; filemark[i++] = getc(FID));
	if (strncmp(filemark, "Rast", 4))
	    error("bad File Mark in Font file.");

	p.p_size = rd2();
	p.p_version = rd1();
	if (p.p_version)
	    error("wrong version of Font file.");
	p.p_glyph = rd3();
	p.p_first = rd2();
	p.p_last = rd2();
	p.p_mag = rd4();
	p.p_desiz = rd4();
	p.p_linesp = rd4();
	p.p_wordsp = rd4();
	p.p_rot = rd2();
	p.p_cadv = rd1();
	p.p_ladv = rd1();
	p.p_id = rd4();
	p.p_res = rd2();

	i = p.p_glyph - 44;
	idstrings = (char *) malloc (i);
	endstring = idstrings;
	while (i--) if ((*(endstring++) = getc(FID)) == EOF)
	    error("bad preamble in Font file.");

	for (i = p.p_first; i <= p.p_last; i++) {
	    g[i].g_height = rd2();
	    g[i].g_width = rd2();
	    g[i].g_up = rd2();
	    g[i].g_left = rd2();
	    g[i].g_pwidth = rd4();
	    g[i].g_bitp = rd3();
	}

	if ((fixtowdth = FIXIN * p.p_res * p.p_mag / 1000.0) == 0.0)
	    fixtowdth = FIXIN * p.p_res;

	if (!msgflag) {
	printf("Font %s, size %.2f, ", argv[1], p.p_desiz * FIX);
	printf("first %d, last %d, res %d, ", p.p_first, p.p_last, p.p_res);
	printf("mag %.2f\n", fixtowdth / (FIXIN * p.p_res));
	printf("spacewidth %.2f, rot %d, cadv %d, ladv %d\n",
		p.p_wordsp * fixtowdth, p.p_rot, p.p_cadv, p.p_ladv);
	while (idstrings < endstring) {
	    for (i = *(idstrings++); i--; ) putchar (*(idstrings++));
	    putchar(':');
	}
	putchar('\n');

	for (i = strlen(argv[1]) + 1; i > 0; --i) putchar(' ');
	printf("ASCII     addr  height  width   up   left   pwidth\n");
	}

	for (i=p.p_first; i<=p.p_last; i++) {
		j = charswanted[i];
		if (i>0 && j==0)
			break;
		if ((gbase = g[j].g_bitp) != 0) {
			if (!msgflag)
			    printf("%s  %3o  %2s  %4d %6d %6d %5d %5d  %6d\n",
				argv[1],  j,  rdchar(j),  gbase,  g[j].g_height,
				g[j].g_width,  g[j].g_up,  g[j].g_left,
				(int) (g[j].g_pwidth * fixtowdth));
			if (verbose || msgflag) {
				int k, l, last;

				H = g[j].g_height;
				W = g[j].g_width;
				lseek(fileno(FID), (long) gbase, 0);
				read(fileno(FID), charbits, (WB = (W+7)/8) * H);
				base = g[j].g_up;
				shozoom();
				if (msgflag) {
					k = g[j].g_width;
					if (zoom == 0) k *= 2;
					else if (zoom == 2) k /= 2;
					curcol += k;
				}
			}
		}
	}
	if (msgflag) {
		for (i=maxline; i>=minline; i--) {
			for (j=0; j<maxcol; j++)
				putchar(msgout[i][j]);
			putchar('\n');
		}
	}
	exit(0);
}

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "ifontinfo: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(8);
};

char *rdchar(c)
char c;
{
	static char ret[3];
	ret[0] = isprint(c) ? ' ' : '^';
	ret[1] = isprint(c) ?  c  : c^0100;
	ret[2] = 0;
	return (ret);
}

int
fbit(row, col)
int row, col;
{
	int thisbyte, thisbit, ret;

	if (row<0 || row>=H || col>=W) return(0);
	thisbyte = charbits[row*WB + (col>>3)] & 0xff;
	thisbit = 0x80 >> (col&7);
	ret = thisbyte & thisbit;
	return (ret != 0);
}


/*
The implementation would work like this:
	zoom level	method
	0		2 chars/pixel, 1 is "[]", 0 is "  ".
	1		2 pixels/char 2x1, using " " "," "'" "|"
	2		8 pixels/char 4x2, using 16x16 table
	3		32 pixels/char 8x4, mapped into (2)
	4 and up	similar, mapped into (2)

The 16x16 table maps a 4x2 pattern into a printing ascii character which
most closely approximates that pattern, e.g. the pattern
	|'
	''
would be represented by the character "[".  I have such a table worked out.

Grainer zoom levels would take the rule of reducing it into a smaller bitmap,
or-ing the bits together.  (e.g. level 3 would take a 2x2 chunk and map it
into a single pixel: 0 if all 4 are 0, 1 otherwise.)  These pixels would be
displayed as in 2.
*/

/*
 * graphtab: a table for rudimentary graphics on ordinary terminals.
 * For each 4x2 bit pattern of the form:
 *	ae
 *	bf
 *	cg
 *	dh
 * form the 4 bit quantities abcd and efgh and get table entry
 *	graphtab[abcd][efgh]
 * to display in that character position.
 *
 * General philosophies: the dh bits are intended for descenders where
 * possible.  Characters with radically different appearance on different
 * terminals (e.g. _ and ^) are avoided.
 *
 * Version 1.0, March 1981, Mark Horton.
 */

char tab1[4] = {
	' ', ',', '\'', '|'
};

char graphtab[16][16] = {
' ', '.', '.', ',', '.', ';', ':', 'j', '\'', ':', ':', ';', '\'', ';', '!', '|',
'.', '.', ':', ',', ';', ';', ';', 'j', '/', ';', ';', ';', 'j', 'j', 'j', 'j',
'.', ',', '~', ',', 'r', '<', 'j', 'q', '/', ';', 'I', ';', '/', '|', 'I', '|',
',', ',', 'r', 'x', '/', '/', '/', 'd', '/', '/', '/', 'd', '/', '/', '/', 'd',
'.', ':', '\\', ';', '-', '=', 'v', 'q', '\'', ':', '<', '|', '\'', ':', '+', '+',
';', ';', '>', ';', '=', '=', 'g', 'g', '\'', ':', 'S', 'S', '/', '/', '/', '+',
':', '\\', '\\', '\\', 'r', '<', 'w', 'q', '/', '<', '6', '4', '/', '/', 'd', '+',
'l', 'L', '+', 'b', 'y', '[', 'p', 'g', '/', '<', '/', '6', '/', '/', '/', '+',
'`', ':', ':', ';', '`', '\\', '\\', '\\', '"', ':', ':', ';', '`', '\\', 'Y', 'T',
';', ';', ';', ';', '`', '2', '>', '\\', ':', '=', ';', ';', '?', '?', ']', ']',
':', ';', ';', ';', '>', '2', '>', '\\', 'F', ';', 'O', ';', '7', '?', ']', '7',
';', ';', ';', ';', '?', '2', '>', 'b', ';', ';', ';', ';', '?', '?', ']', '#',
'\'', '\\', '\\', '\\', '`', '\\', '\\', '\\', '\'', '\'', '<', '5', '"', '"', 'v', 'q',
';', '\\', '\\', '\\', '`', '=', '\\', '\\', '\'', '\'', '5', '5', '"', '?', 'g', 'g',
'I', 'L', 'L', 'L', 'D', '\\', 'b', 'f', 'F', '[', '[', '[', 'P', '?', '#', 'M',
'|', '|', '|', '|', '|', '#', '+', '#', 'T', '[', 'F', 'F', 'P', '?', 'P', 'M'
};


shozoom()
{
	register i;

	if (zoom == 0) 
		sho0();
	else if (zoom == 1)
		sho1();
	else if (zoom == 2)
		sho2();
}

sho0()
{
	register k,l;

	for (k=0; k<H; k++) {
		for (l=0; l<W; l++)
			printf("%s", fbit(k,l)?"[]": "  ");
		printf("\n");
	}
	printf("\n");
}

sho1()
{
	register i,k,l;

	for (k = 0; k < H; k += 2) {
		for(l = 0; l < W; l++) {
			putchar(tab1[(fbit(k,l) << 1) | fbit(k+1,l)]);
		}
		putchar('\n');
	}
	putchar('\n');
}

sho2()
{
	register i,j,k,l;
	int line = curline + (base+3)/4;
	int col;

	k = base%4;
	if (k > 0) k -= 4;
	while (k < H) {
		l = 0;
		col = curcol;
		while (l<W) {
			i = fbit(k,l)*8 + fbit(k+1,l)*4 + 
			    fbit(k+2,l)*2 + fbit(k+3,l);
			l++;
			j = fbit(k,l)*8 + fbit(k+1,l)*4 + 
			    fbit(k+2,l)*2 + fbit(k+3,l);

			if (msgflag) {
				if (graphtab[i][j] != ' ') {
					if (line > maxline) maxline = line;
					if (line < minline) minline = line;
					if (col > maxcol)   maxcol  = col;
				}
				msgout[line][col] = graphtab[i][j];
			} else
				printf("%c",graphtab[i][j]);
			l++;
			col++;
		}
		if (msgflag == 0)
			printf("\n");
		k += 4;
		line--;
	}
	if (msgflag == 0)
		printf("\n");
}

rd1()
{
    int i;

    if((i = getc(FID)) == EOF) error("file read error");
    return i;
}

rd2()
{
    register int i = rd1() << 8;

    return i + rd1();
}

rd3()
{
    register int i = rd2() << 8;

    return i + rd1();
}

rd4()
{
    register int i = rd2() << 16;

    return i + rd2();
}
