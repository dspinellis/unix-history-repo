/* ch2vft.c	1.2	84/02/16
 *
 * Font translation to vfonts (RST format) from character format.
 *
 *	Use:	ch2vft  [ -i ]  charfile  > vfontfile
 *
 *		Takes input from charfile (which must be in the format written
 *	by xxx2ch), converts to rst format and writes to stdout.  If charfile
 *	is missing, stdin is read.  The -i flag tells ch2rst to ignore the
 *	character codes at the start of each glyph definition, and pack the
 *	glyphs in consecutive code positions starting with 0.
 */

#include <stdio.h>
#include <ctype.h>
#include <vfont.h>


#define RES		200	/* for vfont, resolution is 200 */
#define MAXLINE		200
#define GLYPHSPACE	(MAXLINE * MAXLINE)
#define MAGICNO		0436
#define DIRSIZ		256	/* vfonts MUST have 256 entries */
#define DIMLIMIT	128


char *	rdchar();
char *	malloc();

struct dispatch g[DIRSIZ];	/* directory of glyph definitions */
struct header	head;		/* font file header */

int	code, codeindex;
int	width, length, maxv, minv, maxh, minh, refv, refh;
int	fileaddr;

int	ignorecode = 0;
FILE *	filep;
char	ibuff[MAXLINE];
char	ebuff[MAXLINE];
char *	glyphs[DIRSIZ];
char	charbits[GLYPHSPACE];	/* place to store bits for a glyph */


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

    head.magic = MAGICNO;
    head.maxx = head.maxy = head.xtend = 0;
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
	    if (strcmp(ebuff, "size") == 0);
	    else if (strcmp(ebuff, "version") == 0) {
		if ((int) (par + 0.5))
		    error("wrong version (%d) for Font file.", (int)(par+0.5));
	    } else if (strcmp(ebuff, "mag") == 0);
	    else if (strcmp(ebuff, "desiz") == 0);  /*des_size = par/FIX + 0.5*/
	    else if (strcmp(ebuff, "linesp") == 0);
	    else if (strcmp(ebuff, "wordsp") == 0);
	    else if (strcmp(ebuff, "rot") == 0);
	    else if (strcmp(ebuff, "cadv") == 0);
	    else if (strcmp(ebuff, "ladv") == 0);
	    else if (strcmp(ebuff, "id") == 0);
	    else if (strcmp(ebuff, "res") == 0) {
		if (((int) (par + 0.5)) != RES)
		    fprintf(stderr, "ch2vft warning: wrong resolution (%d).\n",
			(int) (par + 0.5) );
	    } else
		error("unknown input descriptor, \"%s\"", ebuff);
	} else {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", ibuff);
	    if (ignorecode) i = codeindex++; else i = code;
	    if (i < 0 || i >= DIRSIZ) error("code (%d) out of range", i);
	    g[i].width = par + 0.5;

	    chp = &charbits[0];
	    if (fgets(chp, MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(chp) - 1;
	    minh = width;
	    maxh = 0;
	    refv = minv = -1;

	    for (length = 0; *chp != '\n'; length++) {
		if ((length + 1) * width > GLYPHSPACE)
		    error ("out of glyph space");
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
			case '*':
				maxv = length;
				if (minv < 0) minv = length;
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

	    if (refv < 0) error("no reference point in glyph %d.", code);
	    if (minv < 0) {
		minv = maxv = refv;
		minh = maxh = refh;
	    }
	    g[i].up = bound(refv - minv);
	    g[i].down = bound(maxv + 1 - refv);
	    g[i].right = bound(maxh + 1 - refh);
	    g[i].left = bound(refh - minh);
	    g[i].nbytes = (maxv + 1 - minv) * ((maxh + 8 - minh) >> 3);

				/* convert from characters to bits */
	    bitp = (glyphs[i] = malloc(g[i].nbytes)) - 1;
	    for (k = minv; k <= maxv; k++) {
		register int bitwidth;

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

    fileaddr = 0;
    for (i = 0; i < DIRSIZ; i++) {
	if (glyphs[i] == (char *) 0) {
	    g[i].nbytes = 0;
	} else {
	    g[i].addr = fileaddr;
	    fileaddr += g[i].nbytes;
	    if (g[i].up > head.maxy) head.maxy = g[i].up;
	    if (g[i].down > head.xtend) head.xtend = g[i].down;
	    if (((int) g[i].left + g[i].right) > head.maxx)
		head.maxx = g[i].left + (int) g[i].right;
	}
    }
    head.size = fileaddr;

    vwrite((char *) &head, sizeof(head));
    vwrite((char *) &(g[0]), sizeof(g));
    for (i = 0; i < DIRSIZ; i++)
	if (glyphs[i] != (char *) 0)
	    vwrite(glyphs[i], g[i].nbytes);
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
			perror("ch2vft: write failed");
			exit(-1);
		}
		usize -= tsize;
	}
}


/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "ch2vft: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(8);
};

bound(i)
{
	if(i < DIMLIMIT && i >= -DIMLIMIT) return i;
	error ("dimension out of range");
}
