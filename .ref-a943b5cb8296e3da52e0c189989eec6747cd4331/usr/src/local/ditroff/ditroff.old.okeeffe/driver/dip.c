/*	dip.c	1.12	(Berkeley)	85/10/29
 *	dip
 *	driver for impress/imagen canon laser printer
 */

/*
output language from troff:
all numbers are character strings

sn	size in points
fn	font as number from 1-n
in	stipple `font' as number from 1-n
cx	ascii character x
Cxyz	funny char xyz. terminated by white space
Hn	go to absolute horizontal position n
Vn	go to absolute vertical position n (down is positive)
hn	go n units horizontally (relative)
vn	ditto vertically
nnc	move right nn, then print c (exactly 2 digits!)
		(this wart is an optimization that shrinks output file size
		 about 35% and run-time about 15% while preserving ascii-ness)
Dt ...\n	draw operation 't':
	Dt d		line thickness set to d
	Ds d		line style (coordinate bit map) set to d
	Dl x y		line from here by x,y
	Dc d		circle of diameter d with left side here
	De x y		ellipse of axes x,y with left side here
	Da x y r	arc counter-clockwise by x,y of radius r
	D~ x y x y ...	wiggly line by x,y then x,y ...
	Dg x y x y ...	gremlin spline by x,y then x,y ...
	Dp s x y ...	polygon filled with s by x,y then ...
	DP s x y ...	unbordered polygon filled with s by x,y then ...
nb a	end of line (information only -- no action needed)
	b = space before line, a = after
pn	new page begins -- set v to 0
#...\n	comment
x ...\n	device control functions:
	x i	init
	x T s	name of device is s
	x r n h v	resolution is n/inch
		h = min horizontal motion, v = min vert
	x p	pause (can restart)
	x s	stop -- done for ever
	x t	generate trailer
	x f n s	font position n contains font s
	x H n	set character height to n
	x S n	set slant to N

	Subcommands like "i" are often spelled out like "init".
*/

#include	<stdio.h>
#include	<signal.h>
#include	<math.h>
#include	<ctype.h>
#include	"dev.h"
#include	"canon.h"
#include	"rst.h"


/* #define  DEBUGABLE	/* whether or not it'll accept the -d option */
#define  abs(n)		((n) >= 0 ? (n) : -(n))
#define  hmot(n)	hpos += n
#define  hgoto(n)	hpos = n
#define  vmot(n)	vpos += n
#define  vgoto(n)	vpos = n

#define	FATAL	1
#define	BMASK	0377
#define	NFONT	35		/* maximum forever */

#ifndef FONTDIR
#define FONTDIR	"/usr/lib/font";
#endif
#define BITDIR	"/usr/local/lib/ifontt";

				/* BOTTOMTHRESH and DELTATHRESH are used to */
				/* search through the glyphs downloaded to */
				/* determine which ones to keep and which to */
				/* dump.  They're tested against BOTTOMTHRESH */
				/* first, then if THAT doesn't release enough */
				/* space, DELTATHRESH is added until it is. */
#define BOTTOMTHRESH	16
#define DELTATHRESH	16
#define MEMSIZE	70000		/* amount of memory inside imagen */
#define BUFFER	20000		/* imagen memory set aside for page buffer */
#define CHARRAY	128		/* size of character use count array */

int	MAXX = (RES*8+RES/3);	/* size of the page... (not 8-1/2" x 11", */
int	MAXY = (RES*10+RES/2+RES/4);		/*  but 8-1/3" x 10-3/4") */

int	output	= 0;		/* do we do output at all? */
int	pageno	= -1;		/* output page number */
int	nolist	= 0;		/* output page list if > 0 */
int	olist[20];		/* pairs of page numbers */

struct dev dev;
struct font *fontbase[NFONT+1];
short *	pstab;
int	nsizes = 1;
int	nfonts;
int	nstips;
int	nchtab;
char *	chname;
short *	chtab;
unsigned char *	fitab[NFONT+1];		/* legal characters for each font */
unsigned char *	widtab[NFONT+1];	/* width table for each font */
unsigned char *	codetab[NFONT+1];	/* device code translation */
char *	fontname[NFONT+1];		/* what font is on what position? */

#ifdef DEBUGABLE
int	dbg	= 0;
#endif

FILE *	tf = stdout;		/* output file pointer */
char *	fontdir = FONTDIR;
char *	bitdir = BITDIR;
FILE *	fp = stdin;		/* input file pointer */

int	totglyph= 0;		/* total space used by glyphs sent down */
int	maxglyph= MEMSIZE - BUFFER;		/* maximum space for glyphs */

int	size = 1;
int	font = 1;
int	stip = 1;
int	family;
int	hpos;		/* current horizontal position (left = 0) */
int	vpos;		/* current vertical position (down positive) */
int	lastw	= 0;	/* width of last input character */
extern int polyborder;		/* flag to turn off borders around a polygon */

typedef struct {
	int	font;
	int	size;
	short	first;
	short	last;
	unsigned char chused[CHARRAY];	/* test array - character downloaded? */
	glyph_dir *glyph;		/* array of character descriptions */
	unsigned char *cdp;		/* char data pointer */
} fontset;

fontset	*fs;			/* A global pointer to the current family */
fontset fontdata[NFONT+1];	/* table of family data descripters */

int	lastsize	= -1;
int	lastfont	= -1;
int	lastx		= -1;
int	lasty		= -1;
int	lastfam		= -1;
int	laststip	= -1;
int	laststipmem	= -1;



main(argc, argv)
char *argv[];
{
	int i;
	char *mktemp();
	char *operand();

	while (--argc > 0 && **++argv == '-') {
		switch ((*argv)[1]) {
		case 'X':
			MAXX = atoi(operand(&argc, &argv));
			break;
		case 'Y':
			MAXY = atoi(operand(&argc, &argv));
			break;
		case 'F':
			fontdir = operand(&argc, &argv);
			break;
		case 'f':
			bitdir = operand(&argc, &argv);
			break;
		case 'o':
			outlist(operand(&argc, &argv));
			break;
		case 'b':
			if ((i = atoi(operand(&argc, &argv))) < 1000) i = 1000;
			else if (i > MEMSIZE - 1000) i = MEMSIZE - 1000;
			maxglyph = MEMSIZE - i; 
			break;
#ifdef DEBUGABLE
		case 'd':
			dbg = atoi(operand(&argc, &argv));
			if (dbg == 0) error (FATAL, "no debug value");
			break;
#endif
		}
	}

	if (argc < 1)
		conv(stdin);
	else
		while (argc-- > 0) {
			if (strcmp(*argv, "-") == 0)
				fp = stdin;
			else if ((fp = fopen(*argv, "r")) == NULL)
				error(FATAL, "can't open %s", *argv);
			conv(fp);
			fclose(fp);
			argv++;
		}

	t_wrapup();
	exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * operand (& argc,  & argv)
 |
 | Results:	returns address of the operand given with a command-line
 |		option.  It uses either "-Xoperand" or "-X operand", whichever
 |		is present.  The program is terminated if no option is present.
 |
 | Side Efct:	argc and argv are updated as necessary.
 *----------------------------------------------------------------------------*/

char *operand(argcp, argvp)
int * argcp;
char ***argvp;
{
	if ((**argvp)[2]) return(**argvp + 2); /* operand immediately follows */
	if ((--*argcp) <= 0) {			/* no operand */
	    error (FATAL, "command-line option operand missing.");
	}
	return(*(++(*argvp)));			/* operand next word */
}


outlist(s)	/* process list of page numbers to be printed */
register char *s;
{
	register int n1, n2;

	nolist = 0;
	while (*s) {
		n1 = 0;
		if (isdigit(*s))
			do
				n1 = 10 * n1 + *s++ - '0';
			while (isdigit(*s));
		else
			n1 = -9999;
		n2 = n1;
		if (*s == '-') {
			s++;
			n2 = 0;
			if (isdigit(*s))
				do
					n2 = 10 * n2 + *s++ - '0';
				while (isdigit(*s));
			else
				n2 = 9999;
		}
		olist[nolist++] = n1;
		olist[nolist++] = n2;
		if (*s != '\0')
			s++;
	}
	olist[nolist] = 0;
#ifdef DEBUGABLE
	if (dbg)
		for (i=0; i<nolist; i += 2)
			printf("%3d %3d\n", olist[i], olist[i+1]);
#endif
}


in_olist(n)	/* is n in olist? */
int n;
{
	int i;

	if (nolist == 0)
		return(1);	/* everything is included */
	for (i = 0; i < nolist; i += 2)
		if (n >= olist[i] && n <= olist[i+1])
			return(1);
	return(0);
}


conv(fp)
register FILE *fp;
{
	register int c;
	register int k;
	int m, n, n1, m1;
	char str[100], buf[300];

	while ((c = getc(fp)) != EOF) {
		switch (c) {
		case '\n':	/* when input is text */
		case ' ':
		case 0:		/* occasional noise creeps in */
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* two motion digits plus a character */
			hmot((c-'0')*10 + getc(fp)-'0');
			put1(getc(fp));
			break;
		case 'c':	/* single ascii character */
			put1(getc(fp));
			break;
		case 'C':
			fscanf(fp, "%s", str);
			put1s(str);
			break;
		case 'D':	/* draw function */
			if (fgets(buf, sizeof(buf), fp) == NULL)
			    error(FATAL, "unexpected end of input");
			switch (buf[0]) {
			case 'l':	/* draw a line */
				sscanf(buf+1, "%d %d", &n, &m);
				drawline(n, m, ".");
				break;
			case 'c':	/* circle */
				sscanf(buf+1, "%d", &n);
				drawcirc(n);
				break;
			case 'e':	/* ellipse */
				sscanf(buf+1, "%d %d", &m, &n);
				drawellip(m, n);
				break;
			case 'a':	/* arc */
				sscanf(buf+1, "%d %d %d %d", &n, &m, &n1, &m1);
				drawarc(n, m, n1, m1);
				break;
			case 'P':
				polyborder = 0;		/* borderless polygon */
			case 'p':	/* polygon */
				sscanf(buf+1, "%d", &m);/* get stipple */
				n = 1;			/* number first */
				while (buf[++n] == ' ');
				while (isdigit(buf[n])) n++;
				setfill(m);		/* set up stipple */
				drawwig(buf+n, fp, -1);	/* draw polygon */
				polyborder = 1;		/* assume polygons */
				break;			/*   all have borders */

			case 'g':	/* gremlin curve */
				drawwig(buf+1, fp, 0);
				break;
			case '~':	/* wiggly line */
				drawwig(buf+1, fp, 1);
				break;
			case 't':	/* line-thickness */
				sscanf(buf+1, "%d", &n);
				drawthick(n);
				break;
			case 's':	/* line-style */
				sscanf(buf+1, "%d", &n);
				drawstyle(n);
				break;
			default:
				error(FATAL, "unknown drawing function %s",buf);
				break;
			}
			break;
		case 's':
			fscanf(fp, "%d", &n);	/* ignore fractional sizes */
			setsize(t_size(n));
			break;
		case 'f':
			fscanf(fp, "%s", str);
			setfont(t_font(str));
			break;
		case 'i':
			fscanf(fp, "%d", &n);
			setstip(n);
			break;
		case 'H':	/* absolute horizontal motion */
			/* fscanf(fp, "%d", &n); */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hgoto(k);
			break;
		case 'h':	/* relative horizontal motion */
			/* fscanf(fp, "%d", &n); */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hmot(k);
			break;
		case 'w':	/* word space */
			break;
		case 'V':
			fscanf(fp, "%d", &n);
			vgoto(n);
			break;
		case 'v':
			fscanf(fp, "%d", &n);
			vmot(n);
			break;
		case 'p':	/* new page */
			fscanf(fp, "%d", &n);
			t_page(n);
			break;
		case 'n':	/* end of line */
			hpos = 0;

		case '#':	/* comment */
			do
				c = getc(fp);
			while (c != '\n' && c != EOF);
			break;
		case 'x':	/* device control */
			if (devcntrl(fp)) return;
			break;
		default:
			error(FATAL, "unknown input character %o %c", c, c);
		}
	}
}


int devcntrl(fp)	/* interpret device control functions */
FILE *fp;		/* returns -1 upon "stop" command */
{
        char str[20], str1[50], buf[50];
	int c, n;

	fscanf(fp, "%s", str);
	switch (str[0]) {	/* crude for now */
	case 'i':	/* initialize */
		fileinit();
		t_init();
		break;
	case 'T':	/* device name */
	case 't':	/* trailer */
	case 'p':	/* pause -- can restart */
		break;
	case 's':	/* stop */
		return -1;
	case 'r':	/* resolution assumed when prepared */
		fscanf(fp, "%d", &n);
		if (n!=RES) error(FATAL,"Input computed for wrong printer");
		break;
	case 'f':	/* font used */
		fscanf(fp, "%d %s", &n, str);
		fgets(buf, sizeof buf, fp);	/* in case there's a filename */
		ungetc('\n', fp);	/* fgets goes too far */
		str1[0] = 0;	/* in case there's nothing to come in */
		sscanf(buf, "%s", str1);
		loadfont(n, str, str1);
		break;
	case 'H':	/* char height */
		fscanf(fp, "%d", &n);
		t_charht(n);
		break;
	case 'S':	/* slant */
		fscanf(fp, "%d", &n);
		t_slant(n);
		break;
	}
	while ((c = getc(fp)) != '\n')	/* skip rest of input line */
		if (c == EOF)
			return -1;
	return 0;
}


fileinit()	/* read in font and code files, etc. */
{
	register int i;
	register int fin;
	register int nw;
	register unsigned char *filebase;
	register unsigned char *p;
	unsigned char *malloc();
	char temp[100];

		/* open table for device,
		 * read in resolution, size info, font info, etc.
		 * and set params
		 */

	sprintf(temp, "%s/devip/DESC.out", fontdir);
	if ((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open tables for %s", temp);
	read(fin, &dev, sizeof(struct dev));
	nfonts = dev.nfonts;
	nstips = dev.nstips;
	nsizes = dev.nsizes;
	nchtab = dev.nchtab;
	filebase = malloc(dev.filesize);	/* enough room for whole file */
	read(fin, filebase, dev.filesize);	/* all at once */
	pstab = (short *) filebase;
	chtab = pstab + nsizes + 1;
	chname = (char *) (chtab + dev.nchtab);
	p = (unsigned char *) chname + dev.lchname;
	for (i = 1; i <= nfonts; i++) {
		fontbase[i] = (struct font *) p;
		nw = *p & BMASK;		/* 1st thing is width count */
		p += sizeof(struct font);
		widtab[i] = p;			/* then width table */
		codetab[i] = p + 2 * nw;	/* then code conversion table */
		fitab[i] = p + 3 * nw;		/* then font inclusion table */
		p += 3 * nw + dev.nchtab + 128 - 32;
		t_fp(i, fontbase[i]->namefont, fontbase[i]->intname);
#ifdef DEBUGABLE
		if (dbg > 1) fontprint(i);
#endif
	}
	for (i = 1; i <= nstips; i++) {		/* add in Stipple "filenames" */
		if (nfonts + i <= NFONT)
		    t_fp(nfonts + i, p, (char *)0);
		p += strlen(p) + 1;
	}
	fontbase[0] = NULL;
	close(fin);				/* no fonts loaded yet */
	for (i = 0; i <= NFONT; i++) fontdata[i].font = fontdata[i].size = -1;
}


#ifdef DEBUGABLE
fontprint(i)	/* debugging print of font i (0,...) */
{
	int j, n;
	char *p;

	printf("font %d:\n", i);
	p = (char *) fontbase[i];
	n = fontbase[i]->nwfont & BMASK;
	printf("base=0%o, nchars=%d, spec=%d, name=%s, widtab=0%o, fitab=0%o\n",
		p, n, fontbase[i]->specfont, fontbase[i]->namefont, widtab[i], fitab[i]);
	printf("widths:\n");
	for (j=0; j <= n; j++) {
		printf(" %2d", widtab[i][j] & BMASK);
		if (j % 20 == 19) printf("\n");
	}
	printf("\ncodetab:\n");
	for (j=0; j <= n; j++) {
		printf(" %2d", codetab[i][j] & BMASK);
		if (j % 20 == 19) printf("\n");
	}
	printf("\nfitab:\n");
	for (j=0; j <= dev.nchtab + 128-32; j++) {
		printf(" %2d", fitab[i][j] & BMASK);
		if (j % 20 == 19) printf("\n");
	}
	printf("\n");
}
#endif


loadfont(n, s, s1)	/* load font info for font s on position n (0...) */
int n;
char *s, *s1;
{
	char temp[60];
	int fin, nw;

	if (n < 0 || n > NFONT)
		error(FATAL, "illegal fp command %d %s", n, s);
	if (fontbase[n] != NULL && strcmp(s, fontbase[n]->namefont) == 0)
		return;

	for (fin = 1; fin <= NFONT; fin++)	/* first check to see if the */
	    if (strcmp(s, fontbase[fin]->namefont) == 0) {  /* font is loaded */
		register unsigned char *c;		    /* somewhere else */

#define ptrswap(x, y) { c = (unsigned char*) (x); x = y; y = c; }
#define ptrfswap(x, y) { c=(unsigned char*)(x); x = y; y = (struct font *) c; }

		ptrfswap(fontbase[n], fontbase[fin]);
		ptrswap(codetab[n], codetab[fin]);
		ptrswap(widtab[n], widtab[fin]);
		ptrswap(fitab[n], fitab[fin]);
		t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
		t_fp(fin, fontbase[fin]->namefont, fontbase[fin]->intname);
		return;
	    }

	if (s1 == NULL || s1[0] == '\0')
		sprintf(temp, "%s/devip/%s.out", fontdir, s);
	else
		sprintf(temp, "%s/%s.out", s1, s);
	if ((fin = open(temp, 0)) < 0) {
		error(!FATAL, "can't open font table %s", temp);
		return;
	}
	if (fontbase[n] != NULL)
		free(fontbase[n]);
	fontbase[n] = (struct font *) malloc(3*255 + dev.nchtab +
				(128-32) + sizeof(struct font));
	if (fontbase[n] == NULL)
		error(FATAL, "Out of space in loadfont %s", s);
	read(fin, fontbase[n], 3*255 + nchtab+128-32 + sizeof(struct font));
	close(fin);
	nw = fontbase[n]->nwfont & BMASK;
	widtab[n] = (unsigned char *) fontbase[n] + sizeof(struct font);
	codetab[n] = (unsigned char *) widtab[n] + 2 * nw;
	fitab[n] = (unsigned char *) widtab[n] + 3 * nw;
	t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
#ifdef DEBUGABLE
	if (dbg > 1) fontprint(n);
#endif
}


/*VARARGS2*/
error(f, s, a1, a2, a3, a4, a5, a6, a7)
int f;
char *s;
{
	fprintf(stderr, "dip: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		exit(2);
}


t_init()	/* initialize device */
{
	drawthick(3);		/* set the line thickness parameter */
	hpos = vpos = 0;
	setsize(t_size(10));	/* start somewhere */
}


/*----------------------------------------------------------------------------*
 | Routine:	t_page ( page_number )
 |
 | Results:	mark this page done for printing.  If we think we've filled
 |		the imagen too much, delete some of the info in the glyph cache.
 |		This is a good time to do this since it's at the end of a page
 |		and will get done every so often.
 *----------------------------------------------------------------------------*/

t_page(pg)	/* do whatever new page functions */
{
	register int i;
	register int threshold;

	pageno = pg;
#ifdef DEBUGABLE
	if(dbg)fprintf(stderr, "t_page %d, output=%d\n", pg, output);
#endif
	if (output != 0)
		putc(AEND, tf);
	output = in_olist(pg);

	if (output) {
	    threshold = BOTTOMTHRESH;
	    while (totglyph >= maxglyph) {
		for (i = 0; i < NFONT; i++) {
		    if (fontdata[i].font != -1)
			clearglyphs(i, threshold);
		}
		threshold += DELTATHRESH;
	    }
	}
	lastx = lasty = -1;
	hpos = vpos = 0;
}


t_size(n)	/* convert integer to internal size number*/
int n;
{
	int i;

	if (n <= pstab[0])
		return(0);
	else if (n >= pstab[nsizes-1])
		return(nsizes-1);
	for (i = 0; n > pstab[i]; i++)
		;
	return(i);
}


t_charht(n)	/* set character height to n */
int n;
{
	/* punt for now */
}


t_slant(n)	/* set slant to n */
int n;
{
	/* punt for now */
}


t_font(s)	/* convert string to internal font number */
char *s;
{
	int n;

	n = atoi(s);
	if (n < 0 || n > nfonts)
		n = 1;
	return(n);
}


t_wrapup()
{
	putc(AEND, tf);
	putc(AEOF, tf);
}


put1s(s)	/* s is a funny char name */
register char *s;
{
	static int i = 0;

	if (!output)
		return;
#ifdef DEBUGABLE
	if (dbg) printf("%s ", s);
#endif
	if (strcmp(s, &chname[chtab[i]]) != 0)
		for (i = 0; i < nchtab; i++)
			if (strcmp(&chname[chtab[i]], s) == 0)
				break;
	if (i < nchtab)
		put1(i + 128);
	else
		i = 0;
}


put1(c)	/* output char c */
register int c;
{
	register unsigned char *pw;
	register unsigned char *p;
	register int i;
	register int j;
	register int k;
	int ofont, code;

	if (!output)
		return;
	c -= 32;
	if (c <= 0) {
#ifdef DEBUGABLE
		if (dbg) printf("non-exist 0%o\n", c+32);
#endif
		return;
	}
	ofont = font;
	i = fitab[font][c];
	if (i != 0) {	/* it's on this font */
		p = codetab[font];
		pw = widtab[font];
	} else {		/* on another font */
		k = font;	/* start with current, then run down the list */
		for (j=0; j++ <= nfonts; k = (k+1) % (nfonts+1))
			if (fontbase[k] != NULL && (i = fitab[k][c]) != 0) {
				p = codetab[k];
				pw = widtab[k];
				setfont(k);
				break;
			}
	}
	code = p[i] & BMASK;
	if (i == 0) {
#ifdef DEBUGABLE
		if (dbg) printf("not found 0%o\n", c+32);
#endif
		return;
	}
	lastw = (pw[i] * pstab[size] + dev.unitwidth/2) / dev.unitwidth;
#ifdef DEBUGABLE
	if (dbg) {
		if (isprint(c+32))
			printf("%c %d\n", c+32, code);
		else
			printf("%03o %d\n", c+32, code);
	} else
#endif
		if (output) xychar(code);
	if (font != ofont)
		setfont(ofont);
}


setsize(n)	/* set point size to n (internal) */
int n;
{
	size = n;
}


/*----------------------------------------------------------------------------*
 | Routine:	t_fp ( number, string, string_internal )
 |
 | Results:	font position number now contains font 'string', internal
 |		font name (number) is ignored.
 |
 | Side Efct:	any fonts loaded into fontdata with this font number are
 |		removed.  And, to make sure they're not accessed, if lastfont
 |		equals number, it is "disabled" by setting lastfont to -1.
 *----------------------------------------------------------------------------*/

t_fp(n, s, si)
int n;
char *s, *si;
{
	register int i;

	fontname[n] = s;
	for (i = 0; i <= NFONT; i++)		/* release any font files */
		if (fontdata[i].font == n) {	/* for this font */
			clearglyphs (i, 1000);
			putc(AFORCE, tf);
			free (fontdata[i].cdp);
			free (fontdata[i].glyph);
			fontdata[i].font = -1;
		}
	if (n == lastfont) lastfont = -1;
}


setfont(n)	/* set font to n */
int n;
{
	if (!output)
		return;
	if (n < 0 || n > nfonts)
		error(FATAL, "illegal font %d", n);
	font = n;
}


setstip(n)	/* set stipple "font" to n */
int n;
{
	if (!output)
		return;
	if (n < 1 || n > nstips)
		error(FATAL, "illegal stipple %d", n);
	stip = n;
}


/*----------------------------------------------------------------------------*
 | Routine:	rd1, rd2, rd3, rd4 ( file_pointer )
 |
 | Results:	gets one, two three or four bytes from a file and interprets
 |		them as integers.  Most significant bytes come first.
 *----------------------------------------------------------------------------*/

int rd1(fp)
FILE *fp;
{
    register int i;

    if((i = getc(fp)) == EOF) error(FATAL, "font file read error");
    return i;
}

int rd2(fp)
FILE *fp;
{
    register short i = rd1(fp) << 8;

    return i | rd1(fp);
}

int rd3(fp)
FILE *fp;
{
    register int i = rd2(fp) << 8;

    return i | rd1(fp);
}

int rd4(fp)
FILE *fp;
{
    register int i = rd2(fp) << 16;

    return i | rd2(fp);
}


/*----------------------------------------------------------------------------*
 | Routine:	getfontdata ( font, size )
 |
 | Results:	returns the family number of the font/size found.  If the
 |		particular point size requested is not found, other sizes are
 |		searched for.  The font information pointer, fs, is set to
 |		point to data for "font" at point size "size".  If no infor-
 |		mation for that font is available, the info is read in from
 |		the appropriate font file.  The table "fontdata" holds all the
 |		fonts, and it is cleared of a random font/size if necessary.
 *----------------------------------------------------------------------------*/

int getfontdata(f, s)
int f;
int s;
{
	char name[100];
	register FILE *fd;
	register int i;
	register int fam;
	register int bitbase;
	register glyph_dir *maxgp;
	register glyph_dir *gp;
	preamble p;

				/* first check if it's here already */
	for (fam = 0; fam <= NFONT; fam++)
	    if (fontdata[fam].font == f && fontdata[fam].size == s) {
		fs = &fontdata[fam];
		return (fam);
	    }
						/* find an empty slot */
	for (fam = 0; fam < NFONT && fontdata[fam].font != -1; fam++);
	fs = &fontdata[fam];
	if (fs->font != -1) {		/* clear a slot if not empty */
		clearglyphs(fam, 1000);		/* dumb version - always take */
		putc(AFORCE, tf);		/* the last one to replace */
		free(fs->glyph);
		free(fs->cdp);
	}

	bitbase = s;
			/* try to open font file - if unsuccessful, hunt for */
			/* a file of same style, different size to substitute */
	i = -1;	 /* direction to look in pstab (smaller first) */
	do {
	    sprintf(name, "%s/%s.%d", bitdir, fontname[f], pstab[bitbase]);
	    fd = fopen(name, "r");
	    if (fd == NULL) {		/* File wasn't found. Try another ps */
		bitbase += i;
		if (bitbase < 0) {	/* past beginning - look higher */
		    i = 1;
		    bitbase = s + i;
		}
		if (bitbase > nsizes)	/* past top - forget it */
		    i = 0;
	    }
	} while (fd == NULL && i != 0);

	if (fd == NULL)			/* completely unsuccessful */
		error(FATAL,"can't open %s/%s.%d",bitdir,fontname[f],pstab[s]);
						/* check for proper file mark */
	for(i = 0; i < FMARK; filemark[i++] = getc(fd));
	if (strncmp(filemark, "Rast", 4))
		error(FATAL, "bad File Mark in %s.", name);
					/* get preamble */
	p.p_size = rd2(fd);
	p.p_version = rd1(fd);
	if (p.p_version)
		error(FATAL, "wrong version of Font file: %s.", name);
	p.p_glyph = rd3(fd);
	fs->first = p.p_first = rd2(fd);
	fs->last = p.p_last = rd2(fd);
				/* skip rest of preamble */
	i = p.p_glyph - 18;
	while (i--) getc(fd);
	fs->glyph = (glyph_dir *)	/* allocate first */
		((char *) malloc((p.p_last - p.p_first + 1) * sizeof(glyph_dir))
		- (char *) (p.p_first * sizeof(glyph_dir)));
	maxgp = gp = &(fs->glyph[p.p_first]);
	bitbase = maxgp->g_bitp;
	for (i = p.p_first; i++ <= p.p_last; gp++) {
	    gp->g_height = rd2(fd);
	    gp->g_width = rd2(fd);
	    gp->g_up = rd2(fd);
	    gp->g_left = rd2(fd);
	    gp->g_pwidth = rd4(fd);
	    if ((gp->g_bitp = rd3(fd)) > maxgp->g_bitp)	/* find the glyphs */
		maxgp = gp;				/* farthest from and */
	    else if(gp->g_bitp < bitbase)		/* nearest to the */
		bitbase = gp->g_bitp;			/* start of the file */
	}
					/* remove file offset in bit pointers */
	for (gp = fs->glyph, i = p.p_first; i++ <= p.p_last; gp++)
	    gp->g_bitp -= bitbase;

	i = maxgp->g_bitp + maxgp->g_height * ((maxgp->g_width + 7) / 8);
	fs->cdp = (unsigned char *) malloc(i);
	lseek(fileno(fd), (long) bitbase, 0);
	if (read(fileno (fd), fs->cdp, i) != i)
		error(FATAL, "can't read in %s", name);
	fclose(fd);

	fs->size = s;
	fs->font = f;
	for (i = 0; i < CHARRAY; fs->chused[i++] = 0);
	return (fam);
}


/*----------------------------------------------------------------------------*
 | Routine:	setfill(stipple_number)
 |
 | Results:	sends the appropriate command to set the fill-pattern
 |		for a particular stipple.  Sends the glyph if necessary,
 |		and does nothing if the pattern is the same.  Takes stipple
 |		font from current "stip" number.
 *----------------------------------------------------------------------------*/

setfill(number)
register int number;
{
	register int fam;
	register int gsize;
	register glyph_dir *par;
	register unsigned char *p;
	register fontset *savefs;

	if (stip == laststip && number == laststipmem)
		return;

	savefs = fs;			/* getfontdata sets fs, so we have to */
					/* save it before calling getfontdata */
	fam = getfontdata(nfonts + stip, nsizes);
	laststip = stip;
	laststipmem = number;		/* must be set before call to polygon */

	if (!number || number < fs->first || number > fs->last) {
nostipbits:
		fs = savefs;		/* forget it if it's out of range */
		laststipmem = 0;	/* force NO stipple */
		return;
	}
	if (fs->chused[number] == 0) {		/* stipple not down-loaded */
		par = &(fs->glyph[number]);
		if (!par->g_bitp)
		    goto nostipbits;
		totglyph += glspace(par);
		putc(ABGLY, tf);
		putint((fam << 7) | number, tf);
 		putint(par->g_pwidth, tf);
		putint(par->g_width, tf);
		putint(par->g_left, tf);
		putint(par->g_height, tf);
		putint(par->g_up, tf);
		gsize = ((par->g_width + 7)/8) * par->g_height;
		p = fs->cdp + par->g_bitp;
		while (gsize--)
			putc(*p++, tf);
	}
						/* mark that it's been used */
	if (fs->chused[number] != BMASK)
		fs->chused[number]++;
	putc(ASTEXTURE, tf);			/* set the texture */
	putint((fam << 7) | number, tf);
	fs = savefs;				/* return fs to proper spot */
}


xychar(c)
register int c;
{
	register unsigned char *p;
	register glyph_dir *par;
	register int gsize;


	if (c >= CHARRAY) {
#ifdef DEBUGABLE
		if (dbg) error(!FATAL, "character out of range: %d 0%o", c, c);
#endif
		return;
	}
	if (font != lastfont || size != lastsize) {
		family = getfontdata(font, size);
		lastsize = size;
		lastfont = font;
	}
	par = &(fs->glyph[c]);
	p = fs->cdp + par->g_bitp;
	if (family != lastfam) {
		putc(AF, tf);
		putc(lastfam = family ,tf);
	}

	if (fs->chused[c] == 0) {	/* 1st use of this character */
		totglyph += glspace(par);
		putc(ABGLY, tf);
		putint((family << 7) | c, tf);
 		putint(lastw, tf);		/* use troff's width, not */
		putint(par->g_width, tf);	/* the RST character width */
		putint(par->g_left, tf);
		putint(par->g_height, tf);
		putint(par->g_up, tf);
		gsize = ((par->g_width + 7)/8) * par->g_height;
		while (gsize--)
			putc(*p++, tf);
	}
					/* note that character's been used */
	if (fs->chused[c] != BMASK)
		fs->chused[c]++;
	hvflush();
	putc(c, tf);		/* guaranteed to be in range */
	lastx += lastw;		/* take account of the automatic advance */
}


/*----------------------------------------------------------------------------*
 | Routine:	hvflush ( )
 |
 | Results:	force current position (hpos, vpos) on the imagen
 *----------------------------------------------------------------------------*/

hvflush()
{
	if (vpos != lasty) {
		putc(ASETV, tf);
		putint(lasty = vpos, tf);
	}
	if (hpos != lastx) {
		putc(ASETH, tf);
		putint(lastx = hpos, tf);
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	glspace ( glyph )
 |
 | Results:	returns how much space the glyph (defined by the glyph_dir
 |		entry) will take in the imagen's memory.
 *----------------------------------------------------------------------------*/

glspace(par)
glyph_dir *par;
{
	return 19 + ((par->g_width + 15) / 16 ) * (par->g_height);
}


/*----------------------------------------------------------------------------*
 | Routine:	clearglyphs ( index, limit )
 |
 | Results:	any glyphs downloaded into the imagen with a "chused" entry
 |		less than "limit" (and > 0) are marked for deletion and their
 |		space is "unrecorded" in totglyph.
 |
 | Bugs:	clearglyphs does NOT check index to make sure the family exists
 *----------------------------------------------------------------------------*/

clearglyphs(index, limit)
int index;
int limit;
{
	register fontset *f = &fontdata[index];
	register int j;

#ifdef DEBUGABLE
	if (dbg) fprintf(stderr, "clear %d family of %d (%d/%d) on page %d\n",
			index, limit, totglyph, maxglyph, pageno);
#endif
	for (j = 0; j < CHARRAY; j++) {
		if (f->chused[j] && f->chused[j] < limit) {
			putc(ADELG, tf);
			putint(index<<7 | j, tf);
			totglyph -= glspace (&(f->glyph[j]));
			f->chused[j] = 0;
		}
	}
}


putint(n, f)
int n;
FILE *f;
{
	putc(n >> 8, f);
	putc(n & 0377, f);
}
