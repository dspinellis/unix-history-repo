/*	ch2qms.c	1.1	87/02/05
 *
 * Font translation to qms-style fonts (QUIC format) from character format.
 *
 *	Use:	ch2qms  [ -i  -s  -b# ]  charfile  > qmsfile
 *
 *		Takes input from charfile (which must be in the format written
 *	by one of the xxx2ch programs), converts to qms' QUIC format (with the
 *	baseline version of the format) and writes to stdout.  If charfile is
 *	missing, stdin is read.  The -i flag tells ch2qms to ignore the
 *	character codes at the start of each glyph definition, and pack the
 *	glyphs in consecutive code positions starting with 0.  The -s flag
 *	forces ch2qms to NOT trim off any white space in the glyph map.  This
 *	is useful to make stipples of fixed size.  The -b flag gives ch2qms
 *	a glyph number to produce a baseline from, replacing it's default
 *	(character #65, `A').  If a "baseline" value isn't given explicitly
 *	in the font, one is calculated by searching for the bottom of the
 *	baseline character.
 */

#include <stdio.h>


#define  MAXLINE	300
#define  RES		300	/* resolution, in pixels per inch */
#define  DIRSIZ		256	/* maximum number of characters in font */
#define  HEADER		"^PY^-\n^F^-\n^DF"


char *calloc();

typedef struct {
	int pwidth;		/* printing width of glyph, in bits */
	int gwidth;		/* width of a glyph, in bits */
	int gheight;		/* height of glyph */
	int hoff;		/* from left edge to reference point */
	int voff; 		/* from top edge to reference point */
	char *glyph;		/* encoded bits of glyph */
} glyphentry;


int	code;			/* code of character working on */
int	width, length;		/* dimensions of glyph bit-map */
int	maxv, minv, maxh, minh;	/* limits of "blackness" in the bit-map */
int	maxup = 0;		/* most movement above and */
int	maxdown = 0;		/*   below the reference points */
int	refv, refh;		/* reference point found in glyph */
int	lineok;

int	ignorecode = 0;		/* option:  ignore the character number given */
int	stipple = 0;		/* option:  use whitespace in the glyph */
int	height = -1;		/* height of every character in the font */
int	fontid;			/* the "font number" */
int	landscape = 0;		/* flag:  is this a landscape font? */
int	version = 0;		/* version code */
int	baseline = -1;		/* baseline of font (if specified) */
int	baselinechar = 65;	/* character to use to figure baseline from */
int	bitwidth;
FILE *	filep;
char	buff[MAXLINE][MAXLINE];
char	name[5];
char *	bitp;
glyphentry g[DIRSIZ];


main(argc, argv)
int argc;
char **argv;
{
    register int i;
    register int j;
    register int codeindex;
    register char *chp;
    float par;

    while (argc > 1 && argv[1][0] == '-') {
	switch(argv[1][1]) {
	case 'b':
		baselinechar = atoi(argv[1] + 2);
		if (baselinechar < 0 || baselinechar >= DIRSIZ)
		    error("baseline character %d out of range", baselinechar);
		break;
	case 'i':
		ignorecode = 1;
		break;
	case 's':
		stipple = 1;
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

    fgets(buff[0], MAXLINE, filep);
    if (strcmp(buff[0], "fontheader\n"))
	error("not a character font file");

    while (fgets(buff[0], MAXLINE, filep) != NULL) {
	if (index(buff[0], '\n') == 0)
	    error("input line too long");

	if (buff[0][0] != ':') {
	    sscanf(buff, "%s %f", buff[1], &par);
	    i = par + 0.5;
	    if (strcmp(buff[1], "rot") == 0)
		switch (i) {
		    case 1:
			landscape = 1;
			break;
		    case 0:
			landscape = 0;
			break;
		    default:
			error("illegal rotation (%d).", i);
		}
	    else if (strcmp(buff[1], "version") == 0) version = i;
	    else if (strcmp(buff[1], "name") == 0)
		sscanf(buff[0], "%s %s", buff[1], name);
	    else if (strcmp(buff[1], "linesp") == 0) height = i;
	    else if (strcmp(buff[1], "id") == 0) fontid = i;
	    else if (strcmp(buff[1], "baseline") == 0) baseline = i;
	    else if (strcmp(buff[1], "res") == 0) {
		if (i != RES)
		  fprintf(stderr,"ch2qms: warning, wrong resolution (%d).\n",i);
	    } /* ignore unrecognized fields */
	} else {
	    if (sscanf (buff[0], ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", buff[0]);
	    if (ignorecode) codeindex++; else codeindex = code;
	    if (codeindex >= DIRSIZ)
		error("glyph number (%d) out of range", codeindex);
	    g[codeindex].pwidth = par + 0.5;

	    if (fgets(buff[0], MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(buff[0]) - 1;
	    minh = width;
	    maxh = 0;
	    refh = minv = -1;

	    for (length = 0; *(chp = &(buff[length][0])) != '\n'; length++) {
		if (length >= MAXLINE)
		    error("not enough space to read in glyph");
		lineok = 0;
		for (i = 0; i <= width; i++, chp++) {
		    switch (*chp) {
			case '\n':
			case '\r':
				lineok = (i == width);
			case '.':
				*chp = 0;
				break;
			case 'x':
				*chp = 0;
			case 'X':
				if (refh >= 0)
				    error ("glyph %d - two reference points",
									code);
				refh = i;
				refv = length;
				if (!*chp) break;
			case '@':
			case '*':
				*chp = '\1';
				if (minv < 0) minv = length;
				if (i < minh) minh = i;
				if (i > maxh) maxh = i;
				maxv = length;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		} /* for i */
		if (!lineok) error("lines not equal length in glyph %d", code);
		if (fgets(buff[length + 1], MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    } /* for length */

	    if (stipple) {		/* use the whole box to make a */
		minv = 0;		/* stipple pattern. */
		minh = 0;
		maxv = length - 1;
		maxh = width - 1;
	    }

	    if (refh < 0) error("no reference point in glyph %d.", code);
	    if (minv < 0) {
		minv = maxv = refv;
		minh = maxh = refh;
	    }
	    if (landscape) {
		if (maxup < width - refh) maxup = width - refh;
		if (maxdown < refh) maxdown = refh;
	    } else {
		if (maxup < refv) maxup = refv;
		if (maxdown < length - refv) maxdown = length - refv;
	    }

	    g[codeindex].gwidth = maxh + 1 - minh;
	    g[codeindex].gheight = maxv + 1 - minv;
	    g[codeindex].hoff = refh - minh;
	    g[codeindex].voff = refv - minv;
	    bitp = calloc(((g[codeindex].gwidth+7)/8)*g[codeindex].gheight,1);
	    g[codeindex].glyph = bitp;
	    bitp--;
	    for (i = minv; i <= maxv; i++) {
		chp = &(buff[i][minh]);
		bitwidth = 0;
		for (j = minh; j <= maxh; j++, chp++) {
		    if (--bitwidth < 0) {
			*++bitp = '\0';
			bitwidth = 7;
		    }
		    if (*chp) *bitp |= 1 << bitwidth;
		}
	    } /* for i */
	} /* else */
    } /* while */

    if (height < 0) {
	height = maxup + maxdown + 1;
    }
    if (baseline < 0) {
	if (g[baselinechar].glyph == (char *) 0)
	    error("no glyph at baseline character %d", baselinechar);
	if (landscape) {
	    i = g[baselinechar].hoff;
	    j = g[baselinechar].gwidth;
	    if (i < -1 || i > j / 3) {
		baseline = j;
		baselinechar = 1 + i;
	    } else {
		baseline = maxup + 1;
		baselinechar = 0;
	    }
	} else {
	    i = g[baselinechar].voff;
	    j = g[baselinechar].gheight;
	    if (i < j - 1 || i > j + 1) {
		baseline = j;
		baselinechar = j - i;
	    } else {
		baseline = maxup + 1;
		baselinechar = 0;
	    }
	}
    } else {
	baselinechar = 0;
    }
    printf(HEADER);
    printf("%05d%c%d%4s%03d%03dT,\n", fontid, landscape ? 'Y' : 'X', version,
	    name, height, baseline);
    baseline -= baselinechar;
    if (landscape) baseline = height - baseline;
    for (codeindex = 0; codeindex < DIRSIZ; codeindex++) {
	if (g[codeindex].glyph != (char *) 0) {
	    code = 80;
	    outhex(codeindex);
	    printf("%03d%03d%03d", g[codeindex].pwidth,
		g[codeindex].gheight, g[codeindex].gwidth);
	    if (landscape) {
		i = -g[codeindex].voff;
		j = baseline - g[codeindex].hoff;
	    } else {
		i = baseline - g[codeindex].voff;
		j = -g[codeindex].hoff;
	    }
	    if (i < 0)
		printf("%04d", i);
	    else
		printf("%03d", i);
	    if (j < 0)
		printf("%04d", j);
	    else
		printf("%03d", j);
	    chp = g[codeindex].glyph;
	    for (j = g[codeindex].gheight; j; j--) {
		if (code > 72) {
		    code = 0;
		    printf("\n");
		}
		for (i = (g[codeindex].gwidth + 7) / 8; i; i--) {
		    outhex(*((unsigned char *) chp++));
		    code += 2;
		}
		if (code % 4) {
		    printf("00");
		    code += 2;
		}
	    }
	    printf(",\n");
	}
    }
    printf("^G\n");
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
	fprintf(stderr, "ch2qms: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}


/*-----------------------------------------------------------------------------
 | Routine:	outhex (number)
 |
 | Results:	prints to standard output, the 2-digit hex value "number"
 |		and does so in capital letters (which printf won't)
 *----------------------------------------------------------------------------*/
char hexness[] = "0123456789ABCDEF";

outhex(value)
int value;
{
    register int i = value;

    printf("%c", hexness[(i>>4)&15]);
    printf("%c", hexness[i&15]);
}
