/* Font translation to Imagen-style fonts (RST format) from character format.
 *
 *	Use:	ch2rst  [ -i ]  charfile  > rstfile
 *
 *		Takes input from charfile (which must be in the format written
 *	by rst2ch), converts to rst format and writes to stdout.  If charfile
 *	is missing, stdin is read.  The -i flag tells ch2rst to ignore the
 *	character codes at the start of each glyph definition, and pack the
 *	glyphs in consecutive code positions starting with 0.
 */

#include <stdio.h>
#include <ctype.h>
#include "rst.h"


#define PREAMBLE	44			/* size of preamble */
#define STRINGS		2			/* length of strings at pre. */
#define STARTGLYPH	PREAMBLE + STRINGS
#define MAXLINE		200
#define wr1(x)		putchar(x)


char *	rdchar();
char *	malloc();

glyph_dir g[DIRSIZ];		/* directory of glyph definitions */
preamble p;			/* set of variables for preamble */
double widthtofix = 1.0 / FIXPIX;	/* fix conversion factor */

int	code, codeindex;
int	width, length, maxv, minv, maxh, minh, refv, refh;
int	bitwidth;

int	ignorecode = 0;
FILE *	filep;
char	ibuff[MAXLINE];
char	ebuff[MAXLINE];
char *	glyphs[DIRSIZ];
char	charbits[20000];	/* place to store bits for a glyph */


main(argc,argv)
int argc;
char **argv;
{
    register int i;
    register int j;
    register int k;
    register char *chp;
    register char *bitp;
    float par;

    while (argc > 1 && argv[1][0] == '-') {
	switch(argv[1][1]) {
	case 'i':
		ignorecode = 1;
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
    for (i = 0; i < DIRSIZ; glyphs[i++] = (char *) 0);

    fgets(ibuff, MAXLINE, filep);
    if (strcmp(ibuff, "header\n"))
	error("not a character font file");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == 0)
	    error("input line too long");

	if (ibuff[0] != ':') {
	    sscanf (ibuff, "%s %f", ebuff, &par);
	    if (strcmp(ebuff, "size") == 0) p.p_size = par + 0.5;
	    else if (strcmp(ebuff, "version") == 0) {
		if (p.p_version = par + 0.5)
		    error("wrong version (%d) for Font file.", p.p_version);
	    } else if (strcmp(ebuff, "mag") == 0) {
		p.p_mag = par + 0.5;
		if (p.p_mag) widthtofix = 1000.0 / (FIXPIX * p.p_mag);
	    } else if (strcmp(ebuff, "desiz") == 0) p.p_desiz = par / FIX + 0.5;
	    else if (strcmp(ebuff, "linesp") == 0)
		p.p_linesp = par * widthtofix + 0.5;
	    else if (strcmp(ebuff, "wordsp") == 0)
		p.p_wordsp = par * widthtofix + 0.5;
	    else if (strcmp(ebuff, "rot") == 0) p.p_rot = par + 0.5;
	    else if (strcmp(ebuff, "cadv") == 0) p.p_cadv = par + 0.5;
	    else if (strcmp(ebuff, "ladv") == 0) p.p_ladv = par + 0.5;
	    else if (strcmp(ebuff, "id") == 0) p.p_id = par + 0.5;
	    else if (strcmp(ebuff, "res") == 0) {
		if ((p.p_res = par + 0.5) != RES)
		    error("wrong resolution in Font file.", p.p_res);
	    } else
		error("unknown input descriptor, \"%s\"", ebuff);
	} else {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", ibuff);
	    if (ignorecode) i = codeindex++; else i = code;
	    g[i].g_pwidth = par * widthtofix + 0.5;

	    chp = &charbits[0];
	    if (fgets(chp, MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(chp) - 1;
	    minh = width;
	    maxh = 0;
	    minv = -1;

	    for (length = 0; *chp != '\n'; length++) {
		for (j = 0; j < width; j++, chp++) {
		    switch (*chp) {
			case '.':
				break;
			case 'x':
				refh = j;
				refv = length;
				*chp = '.';
				break;
			case 'X':
				refh = j;
				refv = length;
			case '@':
			case 'a':
				maxv = length;
				if (minv == -1) minv = length;
				if (j < minh) minh = j;
				if (j > maxh) maxh = j;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		} /* for j */
		if (fgets(chp, MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    } /* for length */
	    g[i].g_height = maxv + 1 - minv;
	    g[i].g_width = maxh + 1 - minh;
	    g[i].g_up = refv - minv;
	    g[i].g_left = refh - minh;
	    g[i].g_bitp = g[i].g_height * ((g[i].g_width + 7) / 8);

	    
	    bitp = (glyphs[i] = malloc(g[i].g_bitp)) - 1;
	    for (k = minv; k <= maxv; k++) {
		chp = &charbits[0] + width * k + minh;
		bitwidth = 0;
		for (j = minh; j <= maxh; j++, chp++) {
		    if (--bitwidth < 0) {
			*++bitp = '\0';
			bitwidth = 7;
		    }
		    if (*chp != '.') *bitp |= 1 << bitwidth;
		}
	    } /* for */
	} /* else */
    } /* while */

    if (ignorecode) {
	i = codeindex;
	p.p_last = i - 1;
	p.p_first = 0;
    } else {
	for (i = DIRSIZ - 1; glyphs[i] == (char *) 0; i--);
	p.p_last = i;
	for (j = 0; glyphs[j] == (char *) 0; j++);
	p.p_first = j;
    }
    bitwidth = STARTGLYPH + 15 * (1 + p.p_last - p.p_first);

    printf("Rast UCB");
    wr2(p.p_size);	 wr1(p.p_version);	wr3(STARTGLYPH);
    wr2(p.p_first);	 wr2(p.p_last);		wr4(p.p_mag);
    wr4(p.p_desiz);	 wr4(p.p_linesp);	wr4(p.p_wordsp);
    wr2(p.p_rot);	 wr1(p.p_cadv);		wr1(p.p_ladv);
    wr4(p.p_id);	 wr2(p.p_res);
    for (i = 0; i < STRINGS; i++) putchar('\0');

    for (i = p.p_first; i <= p.p_last; i++) {
	if (glyphs[i] == (char *) 0) {
	    for (j = 0; j < 15; j++) putchar('\0');
	} else {
	    wr2(g[i].g_height);
	    wr2(g[i].g_width);
	    wr2(g[i].g_up);
	    wr2(g[i].g_left);
	    wr4(g[i].g_pwidth);
	    wr3(bitwidth);
	    bitwidth += g[i].g_bitp;
	}
    }
    fflush(stdout);

    for (i = p.p_first; i <= p.p_last; i++)
	if (glyphs[i] != (char *) 0)
	    vwrite(glyphs[i], g[i].g_bitp);
}


vwrite(buf,usize)
char *buf;
int usize;
{
	int tsize = 0;

	while (usize) {
		buf += tsize;
		tsize = usize > BUFSIZ ? BUFSIZ : usize;
		if ((tsize = write(1, buf, tsize)) < 0) {
			perror("ch2rst: write failed");
			exit(-1);
		}
		usize -= tsize;
	}
}


/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "ch2rst: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(8);
};


wr2(i)
unsigned int i;
{
    wr1((i>>8)&255);
    wr1(i&255);
}

wr3(i)
unsigned int i;
{
    wr1((i>>16)&255);
    wr1((i>>8)&255);
    wr1(i&255);
}

wr4(i)
unsigned int i;
{
    wr1((i>>24)&255);
    wr1((i>>16)&255);
    wr1((i>>8)&255);
    wr1(i&255);
}
