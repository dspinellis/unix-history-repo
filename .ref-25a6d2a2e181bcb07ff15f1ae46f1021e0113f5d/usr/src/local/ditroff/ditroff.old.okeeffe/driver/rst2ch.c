/*	rst2ch.c	(Berkeley)	1.2	86/03/04
 *
 * Font translation for Imagen-style fonts (RST format) to character format.
 *
 *	Use:  rst2ch fontfile  [ character_list ]
 *
 *		Reads "fontfile" from current directory (or if not found,
 *	from BITDIR defined below) and converts it to a character font format
 *	editable by real people, and convertable BACK to rst format by the
 *	ch2rst program.  Output goes to stdout.
 */

#include <stdio.h>
#include "rst.h"

#define  BITDIR		"/usr/src/local/imagen/fonts/raster"

char *	rdchar();
char *	malloc();

char	defascii[DIRSIZ];	/* list of ascii characters - in order */
char	*charswanted = defascii;/* list of characters to print info for */
glyph_dir g[DIRSIZ];		/* directory of glyph definitions */
preamble p;			/* set of variables for preamble */
double	fixtowdth;		/* "fix" and magnification conversion factor */

char	*fontdir = BITDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
FILE *	FID;			/* input file number */

char	charbits[10000];	/* place to store bits for a glyph */
int	gbase;			/* base address of glyphs in RST file */
			/* variables used to print character */
int	H, W, WB, base, lbound, rbound;


main(argc,argv)
int argc;
char **argv;
{
	register int i;
	register int j;
	register int k;
	register int l;


	if (argc < 2 || argc > 3)
		error("usage: %s filename [ charlist ]", argv[0]);

	for (i = 0; i < DIRSIZ; i++)
		defascii[i] = i;
	if (argc >= 3)
		charswanted = argv[2];

	sprintf(IName, "%s/%s", fontdir, argv[1]);
	if ((FID = fopen(argv[1], "r")) == NULL)
		if ((FID = fopen(IName, "r")) == NULL)
			error("can't find %s", argv[1]);

	for (i = 0; i < FMARK; i++) filemark[i] = getc(FID);
	if (strncmp(filemark, "Rast", 4))
	    error("bad File Mark in Font file.");

	p.p_size = rd2();
	p.p_version = rd1();
	if (p.p_version)
	    error("wrong version (%d) of Font file.", p.p_version);
	p.p_glyph = rd3();
	p.p_first = rd2();
	if ((p.p_last = rd2()) >= DIRSIZ)
	    error("too many glyphs (%d) in font.", p.p_last);
	p.p_mag = rd4();

	p.p_desiz = rd4();
	p.p_linesp = rd4();
	p.p_wordsp = rd4();
	p.p_rot = rd2();
	p.p_cadv = rd1();
	p.p_ladv = rd1();
	p.p_id = rd4();
	p.p_res = rd2();

	if ((fixtowdth = FIXIN * p.p_res * p.p_mag / 1000.0) == 0.0)
	    fixtowdth = FIXIN * p.p_res;
	i = p.p_glyph - 44;
	while (i--) if (getc(FID) == EOF)
	    error("bad preamble in Font file.");

	for (i = p.p_first; i <= p.p_last; i++) {
	    g[i].g_height = rd2();
	    g[i].g_width = rd2();
	    g[i].g_up = rd2();
	    g[i].g_left = rd2();
	    g[i].g_pwidth = rd4();
	    g[i].g_bitp = rd3();
	}

	printf("fontheader\nsize %d\nversion %d\n", p.p_size, p.p_version);
	printf("mag %d\ndesiz %.2f\n", p.p_mag, p.p_desiz * FIX);
	printf("linesp %.2f\n", p.p_linesp * fixtowdth);
	printf("wordsp %.2f\n", p.p_wordsp * fixtowdth);
	printf("rot %d\ncadv %d\nladv %d\n", p.p_rot, p.p_cadv, p.p_ladv);
	printf("id %d\nres %d\n", p.p_id, p.p_res);


	for (l = p.p_first; l <= p.p_last; l++) {
	    j = charswanted[l];
	    if (l>0 && j==0) break;
	    if ((gbase = g[j].g_bitp) != 0) {
		printf(":%d, width = %.2f\n", j, g[j].g_pwidth * fixtowdth);
		H = g[j].g_height;
		W = g[j].g_width;
		if (H <= 0 || W <= 0) {
		    g[j].g_up = 0;
		    g[j].g_left = 0;
		}
		lseek(fileno(FID), (long) gbase, 0);
		read(fileno(FID), charbits, (WB = (W+7)/8) * H);
		base = g[j].g_up;
		if ((lbound = g[j].g_left) > 0) lbound = 0;
		if ((rbound = g[j].g_left + 1) < W) rbound = W;
		for (k = g[j].g_up; k < 0; k++) {
		    for (i = lbound; i < rbound; i++)
			printf("%c", k==g[j].g_up && i==g[j].g_left ? 'x':'.');
		    putchar ('\n');
		}
		for (k = 0; k < H; k++) {
		    for (i = g[j].g_left; i < 0; i++)
			printf("%c", k==g[j].g_up && i==g[j].g_left ? 'x':'.');
		    for (i = 0; i < W; i++)
			printf("%c", k==g[j].g_up && i==g[j].g_left ?
				(biton(k,i) ? 'X':'x') : biton(k,i) ? '@':'.');
		    while (i < rbound) {
			printf("%c", k==g[j].g_up && i==g[j].g_left ? 'x':'.');
			i++;
		    }
		    putchar ('\n');
		}
		while (k <= g[j].g_up) {
		    for (i = lbound; i < rbound; i++)
			printf("%c", k==g[j].g_up && i==g[j].g_left ? 'x':'.');
		    putchar ('\n');
		    k++;
		}
		putchar ('\n');
	    }
	}
	exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	error (format_string, argument1, argument2.... )
 |
 | Results:	fprints a message to standard error, then exits with error
 |		code 1.
 |
 | Side Efct:	This routine does NOT return
 *----------------------------------------------------------------------------*/

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "rst2ch: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit (1);
}


/*----------------------------------------------------------------------------*
 | Routine:	biton (row, column)
 |
 | Results:	returns true (non-0) or false (0) for whether the bit at
 |		(row, col) in the character bit-map is "on".
 *----------------------------------------------------------------------------*/

biton(row, col)
int row, col;
{
	return ((charbits[row * WB + (col >> 3)] & 0xff) & (0x80 >> (col & 7)));
}


/*----------------------------------------------------------------------------*
 | Routines:	rd1, rd2, rd3, rd4
 |
 | Results:	returns 1, 2, 3 or 4 bytes from the font file.  Used to read
 |		2-, 3- and 4- byte integers in the proper byte order.
 *----------------------------------------------------------------------------*/

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
