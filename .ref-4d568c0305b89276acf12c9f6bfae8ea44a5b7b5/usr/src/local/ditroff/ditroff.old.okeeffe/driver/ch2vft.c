/*	ch2vft.c	1.6	85/07/03
 *
 * Font translation to vfont format from character format.
 *
 *	Use:	ch2vft  [ -i  -s ]  charfile  > vfontfile
 *
 *		Takes input from charfile (which must be in the format written
 *	by one of the xxx2ch programs), converts to vfont format and writes it
 *	to stdout.  If charfile is missing, stdin is read.  The -i flag tells
 *	ch2vft to ignore the character codes at the start of each glyph
 *	definition, and pack the glyphs in consecutive code positions starting
 *	with 0.  The -s flag forces ch2vft to include the whole bit-map that
 *	defines the glyph.  Normally, it is trimmed of white space.  This is
 *	usefull for making stipple patterns of fixed size.
 */

#include <stdio.h>
#include <vfont.h>


#ifdef sun
#define RES		120	/* for SUN vfont, resolution is 120 */
#else
#define RES		200	/* for vfont, resolution is 200 */
#endif
#define MAXLINE		200
#define GLYPHSPACE	(MAXLINE * MAXLINE)
#define MAGICNO		0436
#define DIRSIZ		256	/* vfonts MUST have 256 entries */
#define DIMLIMIT	128


char *	malloc();

struct dispatch g[DIRSIZ];	/* directory of glyph definitions */
struct header	head;		/* font file header */

int	code;
int	width, length, maxv, minv, maxh, minh, refv, refh;
int	fileaddr;

int	ignorecode = 0;
int	stipple = 0;
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
    register int codeindex;
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
    for (i = 0; i < DIRSIZ; glyphs[i++] = (char *) 0);

    fgets(ibuff, MAXLINE, filep);
    if (strcmp(ibuff, "fontheader\n"))
	error("not a character font file");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == 0)
	    error("input line too long");

	if (ibuff[0] != ':') {
	    sscanf (ibuff, "%s %f", ebuff, &par);
	    if (strcmp(ebuff, "res") == 0) {
		if (((int) (par + 0.5)) != RES)
		    fprintf(stderr, "ch2vft warning: wrong resolution (%d).\n",
			(int) (par + 0.5) );
	    }
	} else {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", ibuff);
	    if (ignorecode) codeindex++; else codeindex = code;
	    if (codeindex < 0 || codeindex >= DIRSIZ)
		error("code (%d) out of range", codeindex);
	    g[codeindex].width = par + 0.5;

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
		for (i = 0; i < width; i++, chp++) {
		    switch (*chp) {
			case '.':
				break;
			case 'x':
				refh = i;
				refv = length;
				*chp = '.';
				break;
			case 'X':
				refh = i;
				refv = length;
			case '@':
			case '*':
				maxv = length;
				if (minv < 0) minv = length;
				if (i < minh) minh = i;
				if (i > maxh) maxh = i;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		} /* for i */
		if (fgets(chp, MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    } /* for length */

	    if (stipple) {		/* force whole box if making stipples */
		minv = 0;
		minh = 0;
		maxv = length - 1;
		maxh = width - 1;
	    }

	    if (refv < 0) error("no reference point in glyph %d.", code);
	    if (minv < 0) {
		minv = maxv = refv;
		minh = maxh = refh;
	    }
	    g[codeindex].up = bound(refv - minv);
	    g[codeindex].down = bound(maxv + 1 - refv);
	    g[codeindex].right = bound(maxh + 1 - refh);
	    g[codeindex].left = bound(refh - minh);
#ifdef sun
	    g[codeindex].nbytes = (maxv+1-minv) * ((maxh+16-minh) / 16) * 2;
#else
	    g[codeindex].nbytes = (maxv + 1 - minv) * ((maxh + 8 - minh) >> 3);
#endif

				/* convert from characters to bits */
	    bitp = (glyphs[codeindex] = malloc(g[codeindex].nbytes)) - 1;
	    for (i = minv; i <= maxv; i++) {
		register int bitwidth;

		chp = &charbits[0] + width * i + minh;
		bitwidth = 0;
		for (j = minh; j <= maxh; j++, chp++) {
		    if (--bitwidth < 0) {
			*++bitp = '\0';
			bitwidth = 7;
		    }
		    if (*chp != '.') *bitp |= 1 << bitwidth;
		}
#ifdef sun
		if (!((bitp - glyphs[codeindex]) & 1)) *++bitp = '\0';
#endif
	    } /* for i */
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
    exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	bound (value)
 |
 | Results:	checks to make sure that the dimensions of a glyph fit into
 |		the vfont format's limitations.  The up, down, right, and left
 |		fields must fit into a byte!), but can be signed.
 *----------------------------------------------------------------------------*/

bound(i)
{
	if(i < DIMLIMIT && i >= -DIMLIMIT) return i;
	error ("dimension out of range");
}


/*----------------------------------------------------------------------------*
 | Routine:	vwrite (buffer, buffer_size)
 |
 | Results:	writes out character array "buffer" of size "buffer_size"
 |		to standard output in small enough chunks that a pipe could
 |		handle them.
 |
 | Bugs:	this routine shouldn't be needed
 *----------------------------------------------------------------------------*/

vwrite(buf, bufsize)
char *buf;
int bufsize;
{
	int tsize = 0;

	while (bufsize) {
		buf += tsize;
		tsize = bufsize > BUFSIZ ? BUFSIZ : bufsize;
		if ((tsize = write(1, buf, tsize)) < 0) {
			perror("ch2vft: write failed");
			exit(-1);
		}
		bufsize -= tsize;
	}
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
	fprintf(stderr, "ch2vft: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}
