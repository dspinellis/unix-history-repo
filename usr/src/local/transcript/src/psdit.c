/*	@(#)psdit.c	1.4 %G%	*/
#ifndef lint
static char Notice[] = "Copyright (c) 1984, 1985 Adobe Systems Incorporated";
static char *RCSID = "$Header: psdit.c,v 2.1 85/11/24 11:50:41 shore Rel $";
#endif
# define XMOD
/*
 * Changed by Edward Wang (edward@ucbarpa.berkeley.edu) to handle graphics,
 * 17 Feb, 87, with code provided by John Coker (john@renoir.berkeley.edu)
 * and Peehong Chen (phc@renoir.berkeley.edu).
 */
/* psdit.c
 *
 * Copyright (c) 1984, 1985 Adobe Systems Incorporated
 *
 * ditroff intermediate file to PostScript translator
 *
 * Original Version: Barry Hayes spring/summer 1984
 * Edit History:
 * Andrew Shore: Sat Nov 23 20:05:26 1985
 * End Edit History.
 *
 * RCSLOG:
 * $Log:	psdit.c,v $
 * Revision 2.1  85/11/24  11:50:41  shore
 * Product Release 2.0
 *
 * Revision 1.8  85/11/23  20:09:44  shore
 * test for termination of included PostScript was bad
 *
 * Revision 1.7  85/11/21  14:23:56  shore
 * added envget check for PSLIBDIR
 *
 * Revision 1.6  85/11/20  00:43:43  shore
 * support for included PostScript
 * big rework on FlushShow, word "breaks"
 * removed FlushFont and made them instant
 * Still no Gremlin support yet
 *
 * Revision 1.5  85/10/03  10:48:09  shore
 * added FlushShow to xf fix !
 *
 * Revision 1.4  85/10/02  16:20:32  shore
 * fixed xf bug
 * mounting a font causes a font switch!
 *
 * Revision 1.3  85/07/09  13:10:20  shore
 * added fclose on map file
 *
 * Revision 1.2  85/05/14  11:24:23  shore
 * added flush to trailer
 * fixed read bug when mounting fonts
 */

/*
output language from troff:
all numbers are character strings

sn	size in points
fn	font as number from 1-n
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
	Dl x y		line from here by x,y
	Dc d		circle of diameter d with left side here
	De x y		ellipse of axes x,y with left side here
	Da x y r	arc counter-clockwise by x,y of radius r
	Dp ...		bordered polygon
	DP ...		unbordered polygon
	D~ x y x y ...	b-spline by x,y then x,y ...
	Dg ...		gremlin curve
	Dz ...		bezier curve
nb a	end of line (information only -- no action needed)
	a = space before line, a = after
w	paddable word space -- no action needed
pn	new page begins -- set v to 0
{	push current environment (font info & location)
}	pop a saved environment
txxxx	print string xxxx using natural widths
#...\n	comment
x ...\n	device control functions:
	x i[nit]	init
	x T s		name of device is s
	x r[es] n h v	resolution is n/inch
			h = min horizontal motion, v = min vert
	x p[ause]	pause (can restart)
	x s[top]	stop -- done for ever
	x t[railer]	generate trailer
	x f[font] n s	font position n contains font s
	x H[eight] n	set character height to n
	x S[slant] n	set slant to N

Adobe Extension for included PostScript:
%
(raw postscript...)
.\n

*/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <pwd.h>
#ifdef SYSV
extern struct passwd *getpwuid();
#endif
#include "transcript.h"

#include "dev.h"

char *malloc();

#define	NFONT	10

/* DIT state consists of: */
int	hpos;		/* current horizontal position */
int	vpos;		/* current vertical position */
int	fontsize;	/* current font size */
int	fontheight;	/* current character height */
int	fontslant;	/* current font slant */
int	font;		/* current font */
int	resolution;	/* device resolution */
int	minhoriz;	/* minimum horizontal motion */
int	minvert;	/* minimum vertical motion */

int	onspecial;
int	specfont;
int	prevfont;
int 	pfont;

/* {} push/pop stack */
#define DSTACK 10
struct ditstack {
	int hpos, vpos, fontsize, fontheight, fontslant, font;
} ditstack[DSTACK];
int dlevel = 0;

#define ErrorTolerance 48
#define PSWID 0x00000FFF
#define ISPSPROC 0x000FF000

/* PSscale is equivalent to (x * PSmag / 72000) + 0.5 */
#define PSmag 16
#define PSscale(x) (((x) + 2250) / 4500)

/* we maintain PS coords with PSmag times the precision */
/* current PS state is: */

int	PSx;		/* current horizontal position */
int	PSy;		/* current vertical position */
int	savex, savey;	/* position of start of current show string */

/* ps move types -- note that XMOVE|YMOVE == XYMOVE ! */
#define NONE	0
#define XMOVE	1
#define YMOVE	2
#define XYMOVE	3

int movepending = NONE;

/* buffer string for show -- save up adjacent chars */
#define SHOWSIZE 400
char showbuf[SHOWSIZE + 3]; /* extras are for quoting */
int showind = 0;	/* index into string of next available byte */
int PSshowlen = 0;	/* size in big units of buffered string */
int nshow = 0;		/* actual number of show chars in showbuf */
int startx;		/* troff starting pos of current string */
int thisw;

/* #define NONE 0 */
#define HMOT	1
#define VMOT	2
#define CPUT	4
#define BRK	8
#define FNT	16
int lastcmd;

int	output = 0;	/* do we do output at all? */
int	nolist = 0;	/* output page list if > 0 */
int	olist[20];	/* pairs of page numbers */
int	spage = 9999;	/* stop every spage pages */
int	scount = 0;
int	stopped = 0;
int	pageno = 0;
int	firstpage = TRUE;

struct dev dev;
struct font *fontbase[NFONT+1];
short	*pstab;
int	dres;	/* resolution from DESC */
int	nsizes; /* number of point sizes from DESC */
int	nfonts; /* number of fonts from DESC */
int	smnt;	/* index of first special font */
int	nchtab;
char	*chname;
short	*chtab;
char	*fitab[NFONT+1];
char	*widthtab[NFONT+1];	/* widtab would be a better name */
char	*codetab[NFONT+1];	/* device codes */

int	*pswidths[NFONT+1]; /* ps width tables */
int	fontdelta[NFONT+1]; /* nonzero if xf overwrites font i */

/* font position info: */
struct {
	char *name;
	int number;
} fontname[NFONT+1];

#define	FATAL	1
#define	BMASK	0377

#ifdef DEBUG
int	dbg = 0;
int	fdbg = 0;
#define debugp(xxx) (dbg != 0 ? (dbg--, printf xxx, fflush(stdout)) : 0)
#else
#define debugp(x)
#endif

char	devname[20] = "psc";

char	*infilename = "stdin"; /* input file name */
char	*prologfile = PSDITPRO;
char	*ditdir = DitDir;

char	*prog;		/* argv[0] - program name */

	/* for curve and polygon drawing */
#define MAXPOINTS	200
double x[MAXPOINTS], y[MAXPOINTS];
int numpoints;

main(argc, argv)
	int argc;
	char *argv[];
{
	FILE *fp;
	int done();

	prog = argv[0];
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'f':
		case 'F':
			if (argv[1][2])
				ditdir = &argv[1][2];
			else {
				ditdir = argv[2];
				argv++;
				argc--;
			}
			break;
		case 'p':
			if (argv[1][2])
				prologfile = &argv[1][2];
			break;
		case 'o':
			outlist(&argv[1][2]);
			break;
		case 'd':
#ifdef DEBUG
			dbg = atoi(&argv[1][2]);
			if (dbg == 0)
				dbg = 1;
#endif DEBUG
			break;
		case 'b': 		/* ignore busy */
			break;
		case 'w': 		/* ignore wait */
			break;
		case 's':
			spage = atoi(&argv[1][2]);
			if (spage <= 0)
				spage = 9999;
			break;
		}
		argc--;
		argv++;
	}

	if (signal(SIGINT, done) == SIG_IGN) {
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	} else {
		signal(SIGQUIT, done);
		signal(SIGHUP, done);
	}
	signal(SIGTERM, done);

	preface();

	if (argc <= 1)
		conv(stdin);
	else
		while (--argc > 0) {
			if (strcmp(*++argv, "-") == 0)
				fp = stdin;
			else if ((fp = fopen(*argv, "r")) == NULL) {
				fprintf(stderr, "%s: can't open %s\n",
					prog, *argv);
				pexit(prog, 2);
			}
			infilename = *argv;
			conv(fp);
			(void) fclose(fp);
		}
	done();
}

/* process list of page numbers to be printed */
outlist(s)
	register char *s;
{
	int n1, n2, i;

	nolist = 0;
	while (*s) {
		n1 = 0;
		if (isdigit (*s))
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
#ifdef DEBUG
	if (dbg)
		for (i = 0; i < nolist; i += 2)
			printf("%3d %3d\n", olist[i], olist[i + 1]);
#endif
}

conv(fp)	/* convert a file */
	register FILE *fp;
{
	register int c, k;
	int m, n, n1, m1;
	char str[100], buf[300];

	while ((c = getc(fp)) != EOF)
		switch (c) {
		case '\n': case ' ': case '\0':
			break;
		case '{': 		/* push down current environment */
			t_push();
			break;
		case '}':
			t_pop();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* two motion digits plus a character */
			hmot((c - '0') * 10 + getc(fp) - '0');
			lastcmd = HMOT;
			put1(getc(fp), (char *) 0);
			lastcmd = CPUT;
			break;
		case 'c': 		/* single ascii character */
			put1(getc(fp), (char *) 0);
			lastcmd = CPUT;
			break;
		case 'C':
			fscanf(fp, "%s", str);
			put1s(str);
			lastcmd = CPUT;
			break;
		case 't': 		/* straight text */
			fgets(buf, sizeof buf, fp);
			t_text(buf);
			lastcmd = CPUT;
			break;
		case 'D': 		/* draw function */
			fgets(buf, sizeof buf, fp);
			switch (buf[0]) {
			case 'l': 	/* draw a line */
				sscanf(buf + 1, "%d %d", &n, &m);
				drawline(n, m);
				break;
			case 'c': 	/* circle */
				sscanf(buf + 1, "%d", &n);
				drawcirc(n);
				break;
			case 'e': 	/* ellipse */
				sscanf(buf + 1, "%d %d", &m, &n);
				drawellip(m, n);
				break;
			case 'a': 	/* arc */
				sscanf(buf + 1, "%d %d %d %d",
					&n, &m, &n1, &m1);
				drawarc(n, m, n1, m1);
				break;
			case '~': 	/* b-spline */
			case 'g': 	/* gremlin curve */
			case 'z': 	/* bezier cubic */
				drawcurve(buf);
				break;
			case 'p': 	/* filled polygon */
			case 'P': 	/* bordered filled polygon */
				drawpoly(buf);
				break;
			case 't': 	/* line thickness */
			case 's': 	/* line style */
				sscanf(buf + 1, "%d", &n);
				printf("%d D%c\n", n, buf[0]);
				break;
			default:
				fprintf(stderr,
					"%s: unknown drawing function %s\n",
					prog, buf);
				exit(2);
			}
			break;
		case 'i':
			fscanf(fp, "%d", &n);
			printf("%d Di\n", n);
			break;
		case 's':
			fscanf(fp, "%d", &n);
			t_size(n);
			lastcmd = FNT;
			break;
		case 'f':
			fscanf(fp, "%s", str);
			setfont(t_font(str));
			lastcmd = FNT;
			break;
		case 'H': 		/* absolute horizontal motion */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do
				k = 10 * k + c - '0';
			while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hgoto(k);
			lastcmd = HMOT;
			break;
		case 'h': 		/* relative horizontal motion */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do
				k = 10 * k + c - '0';
			while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hmot(k);
			lastcmd = HMOT;
			break;
		case 'w':
			FlushShow(1);
			lastcmd = BRK;
			break;
		case 'V':
			fscanf(fp, "%d", &n);
			vgoto(n);
			lastcmd = VMOT;
			break;
		case 'v':
			fscanf(fp, "%d", &n);
			vmot(n);
			lastcmd = VMOT;
			break;
		case 'p': 		/* new page */
			fscanf(fp, "%d", &n);
			t_page(n);
			lastcmd = NONE;
			break;
		case 'n': 		/* end of line -- ignore */
			while (getc(fp) != '\n')
				;
			FlushShow(1);
			lastcmd = BRK;
			break;
		case '#': 		/* comment */
			/* maybe should pass through as a PS comment */
			while (getc(fp) != '\n')
				;
			break;
		case 'x': 		/* device control */
			devcntrl(fp);
			break;
		case '%':		/* imbedded PostScript */
			/* copy everything up to but NOT including a line */
			/* with at single "." */
			FlushShow(0);
			MoveTo();
			DoMove();
			printf("\n%% included PostScript\n");
			while (fgets(buf, sizeof buf, fp) != NULL) {
				if (strcmp(".\n", buf) == 0)
					break;
				fputs(buf, stdout);
			}
			break;
		default:
			fprintf(stderr, "%s: bad input char \\%03o (%c)\n",
				prog, c, c);
			exit(2);
		}
}

/* put in PostScript prolog */
preface()
{
	register FILE *prolog;
	char hostname[256];
	char tempfile[512];
	struct passwd *pswd;
	long clock;
	char *libdir;

	printf("%%!%s\n", COMMENTVERSION);
	pswd = getpwuid(getuid());
	(void) gethostname(hostname, sizeof hostname);
	printf("%%%%Creator: %s:%s (%s)\n", hostname,
		pswd->pw_name, pswd->pw_gecos);
	printf("%%%%Title: %s (ditroff)\n", infilename);
	printf("%%%%CreationDate: %s", (time(&clock), ctime(&clock)));
	printf("%%%%EndComments\n");
	if ((libdir = envget("PSLIBDIR")) == NULL)
		libdir = LibDir;
	mstrcat(tempfile, libdir, prologfile, sizeof tempfile);
	if (copyfile(tempfile, stdout) != 0) {
		fprintf(stderr, "%s: can't copy prolog file %s\n",
			prog, tempfile);
		exit(2);
	}
	printf("ditstart\n");
}

devcntrl(fp)	/* interpret device control functions */
	FILE *fp;
{
	char str[20], str1[50], buf[50];
	int c, n, res, minh, minv;

	fscanf(fp, "%s", str);
	switch (str[0]) {		/* crude for now */
	case 'i': 		/* initialize */
		fileinit();
		t_init();
		lastcmd = NONE;
		break;
	case 'T': 		/* device name */
		/*
		fscanf(fp, "%s", devname);
		if (strcmp(devname, "psc")) {
			fprintf(stderr, "%s: device not psc\n", prog);
			exit(2);
		}
		*/
		printf("(%s)xT\n", devname);
		lastcmd = NONE;
		break;
	case 't': 		/* trailer */
		t_trailer();
		lastcmd = NONE;
		break;
	case 'p': 		/* pause -- can restart */
		t_reset('p');
		lastcmd = NONE;
		break;
	case 's': 		/* stop */
		t_reset('s');
		lastcmd = NONE;
		break;
	case 'r': 		/* resolution assumed when prepared */
		fscanf(fp, "%d %d %d", &res, &minh, &minv);
		t_res(res, minh, minv);
		lastcmd = NONE;
		break;
	case 'f': 		/* font used */
		fscanf(fp, "%d %s", &n, str);
		fgets(buf, sizeof buf, fp);	/* in case theres a filename */
		ungetc('\n', fp);		/* fgets goes too far */
		str1[0] = 0;	/* in case there is nothing to come in */
		sscanf(buf, "%s", str1);
		loadfont(n, str, str1);
		lastcmd = FNT;
		break;
	case 'H': 		/* char height */
		fscanf(fp, "%d", &n);
		t_charht(n);
		lastcmd = FNT;
		break;
	case 'S': 		/* slant */
		fscanf(fp, "%d", &n);
		t_slant(n);
		lastcmd = FNT;
		break;
#ifdef XMOD
	case 'X': {		/* \X command from ditroff */
            int last;
	    char largebuf[128];
	    fscanf (fp, "%1s", str);
	    switch (str[0]) {
		case 'p' :
		    FlushShow(0);MoveTo();DoMove();
		    fgets(largebuf, sizeof(largebuf), fp);
		    last = strlen(largebuf) - 1;
		    if (last >= 0 && largebuf[last] == '\n') {
			ungetc('\n', fp);
			largebuf[last] = ' ';
		    }
		    puts(largebuf);
		    break;
		case 'f' :
		    FlushShow(0);MoveTo();DoMove();
		    if (fscanf(fp, "%s", largebuf) == 1) {
			char *nl = (char *) index(largebuf, '\n');
			if (nl) *nl = '\0';
			includefile(largebuf);
		    } else
			fprintf(stderr, "warning - include cmd w/o path.\n");
		    break;
	    }
	}
	break;
#endif
	}
	/* skip rest of input line */
	while ((c = getc(fp)) != '\n' && c != EOF)
		;
}

#ifdef XMOD
includefile(filenm)
	char *filenm;
{
	FILE *inf;
	int ch, c1, c2, firstch = 0;

	if (!(inf = fopen(filenm, "r"))) {
		fprintf(stderr, "psdit: fopen(%s): ", filenm);
		perror();
		exit(1);
	}
	c1 = fgetc(inf); c2 = fgetc(inf);
	if (c1 != '%' || c2 != '!')
		fprintf(stderr, "psdit: %s not a postscript file.\n", filenm),
		exit(1);

	fputs("%!", stdout);
	while ((ch = fgetc(inf)) != EOF) {
		putchar(ch);
		if (firstch && ch == '%') {
			/* we have to double leading '%'s */
			putchar('%');
		}
		firstch = (ch == '\n');
	}
	fclose(inf);
}
#endif

fileinit()	/* read in font and code files, etc. */
{
	int i, fin, nw;
	char *filebase, *p;
	char temp[60];
	unsigned msize;

	/*
	 * Open table for device,
	 * read in resolution, size info, font info, etc., and set params.
	 */
	sprintf(temp, "%s/dev%s/DESC.out", ditdir, devname);
	if ((fin = open(temp, 0)) < 0) {
		fprintf(stderr, "%s: can't open %s - %s\n",
			prog, devname, temp);
		pexit(prog, 2);
	}
	if (read(fin, (char *) &dev, sizeof (struct dev)) !=
			sizeof (struct dev)) {
		fprintf(stderr, "%s: can't read %s\n", prog, temp);
		pexit(prog, 2);
	}
	dres = dev.res;
	nfonts = dev.nfonts;
	nsizes = dev.nsizes;
	nchtab = dev.nchtab;
	/* enough room for whole file */
	filebase = malloc((unsigned) dev.filesize);
	if (read(fin, filebase, dev.filesize) != dev.filesize) {
		fprintf(stderr, "%s: trouble reading %s\n", prog, temp);
		pexit(prog, 2);
	}
	pstab = (short *) filebase;		/* point size table */
	chtab = pstab + nsizes + 1;		/* char index table */
	chname = (char *) (chtab + dev.nchtab);	/* char name table */
	p = chname + dev.lchname;		/* end of char name table */
	/* parse the preloaded font tables */
	for (i = 1; i <= nfonts; i++) {
		fontdelta[i] = 0;
		fontbase[i] = (struct font *) p;
		nw = *p & BMASK;	/* number of width entries */
		if ((smnt == 0) && (fontbase[i]->specfont == 1))
			smnt = i;		/* first special font */
		p += sizeof (struct font); /* skip header */
		widthtab[i] = p;		/* width table */
		/* kern table is next */
		codetab[i] = p + 2 * nw;	/* device codes */
		fitab[i] = p + 3 * nw;		/* font index table */
		p += 3 * nw + dev.nchtab + (128 - 32);	/* next font */
		t_fp(i, fontbase[i]->namefont, fontbase[i]->intname);
		loadpswidths(i, fontbase[i]->namefont);
		sayload(i, fontbase[i]->namefont, (char *) 0);
#ifdef DEBUG
		if (fdbg > 1)
			fontprint(i);
#endif
	}
	fontdelta[0] = 0;
	msize = 3*255 + dev.nchtab + (128 - 32) + sizeof (struct font);
	fontbase[0] = (struct font *) malloc(msize);
	widthtab[0] = (char *) fontbase[0] + sizeof (struct font);
	fontbase[0]->nwfont = 255;
	close(fin);
}

loadpswidths(i, name)
	int i;
	char *name;
{
	char temp[60];
	register FILE *auxin;
	register int j;
	int cc, wid, funny;

	sprintf(temp, "%s/dev%s/%s.aux", ditdir, devname, name);
	auxin = fopen(temp, "r");
	/* allocate table */
	if (pswidths[i] == NULL)
		pswidths[i] = (int *) malloc(256 * (sizeof (int)));
	/* initialize to not-there */
	for (j = 0; j <= 255; pswidths[i][j++] = -1)
		;
	/* read them in */
	while (fscanf(auxin, "%d %d %d", &cc, &wid, &funny) != EOF)
		pswidths[i][cc] = wid | (funny << 12);
	(void) fclose(auxin);
}

#ifdef DEBUG
fontprint(i)	/* debugging print of font i (0, ...) */
int i;
{
	int j, n;
	char *p;

	printf("font %d:\n", i);
	p = (char *) fontbase[i];
	n = fontbase[i]->nwfont & BMASK;
	printf("base=0%o, nchars=%d, spec=%d, name=%s, widtab=0%o, fitab=0%o\n",
		p, n, fontbase[i]->specfont,
		fontbase[i]->namefont, widthtab[i], fitab[i]);
	printf("widths:\n");
	for (j = 0; j <= n; j++) {
		printf(" %2d", widthtab[i][j] & BMASK);
		if (j % 20 == 19)
			printf("\n");
	}
	printf("\ncodetab:\n");
	for (j = 0; j <= n; j++) {
		printf(" %2d", codetab[i][j] & BMASK);
		if (j % 20 == 19)
			printf("\n");
	}
	printf("\nfitab:\n");
	for (j = 0; j <= dev.nchtab + 128 - 32; j++) {
		printf(" %2d", fitab[i][j] & BMASK);
		if (j % 20 == 19)
			printf("\n");
	}
	printf("\n");
}
#endif

loadfont(n, s, s1) /* load font info for font s on position n */
	int n;
	char *s, *s1;
{
	char temp[60];
	int fin, nw, norig;
	int bcount;

	if (n < 0 || n > NFONT) {
		fprintf(stderr, "%s: illegal fp command %d %s\n", prog, n, s);
		exit(2);
	}
	if (strcmp(s, fontbase[n]->namefont) == 0)
		return;
	if (fontbase[n]->namefont != 0)
		fontdelta[n] = 1;
	if (s1 == NULL || s1[0] == '\0')
		sprintf(temp, "%s/dev%s/%s.out", ditdir, devname, s);
	else
		sprintf(temp, "%s/%s.out", s1, s);
	if ((fin = open(temp, 0)) < 0) {
		fprintf(stderr, "%s: can't open font table %s\n", prog, temp);
		pexit(prog, 2);
	}
	norig = fontbase[n]->nwfont & BMASK;
	bcount = 3 * norig + nchtab + 128 - 32 + sizeof (struct font);
	(void) read(fin, (char *) fontbase[n], bcount);
	if ((fontbase[n]->nwfont & BMASK) > norig) {
		fprintf(stderr, "%s: Font %s too big for position %d\n",
			prog, s, n);
		exit(2);
	}
	close(fin);
	nw = fontbase[n]->nwfont & BMASK;
	widthtab[n] = (char *) fontbase[n] + sizeof (struct font);
	codetab[n] = (char *) widthtab[n] + 2 * nw;
	fitab[n] = (char *) widthtab[n] + 3 * nw;
	t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
	loadpswidths(n, fontbase[n]->namefont);
	sayload(n, s, s1);
	fontbase[n]->nwfont = norig; /* so can later use full original size */
#ifdef DEBUG
	if (fdbg > 1)
		fontprint(n);
#endif
}

sayload(n, s, s1)	/* position n contains font s (internal s1) */
	int n;
	char *s, *s1;
{
	char pass[60];
	FILE *ptrfile;
	char Adobefont[60];

	if (s1 == NULL || s1[0] == '\0')
		sprintf(pass, "%s/dev%s/%s.map", ditdir, devname, s);
	else
		sprintf(pass, "%s/%s.map", s1, s);
	if ((ptrfile = fopen(pass, "r")) == NULL) {
		fprintf(stderr, "%s: can't open font map file %s\n",
			prog, pass);
		pexit(prog, 2);
	}
	fscanf(ptrfile, "%s", Adobefont);
	FlushShow(0);
	printf("%d(%s)xf %d f\n", n, Adobefont, n);
	font = n;
	(void) fclose(ptrfile);
}

done()
{
	t_reset('s');
	exit(0);
}

t_init()	/* "x i" - initialize device */
{
	movepending = NONE;
	savex = savey = 0;

	t_size(10);		/* start somewhere */
	t_slant(0);
	setfont(1);		/* set font */
	printf("xi\n");
	printf("%%%%EndProlog\n");
}

t_push()	/* begin a new block */
{
	FlushShow(1);
	MoveTo();
	DoMove();
	if (dlevel == DSTACK) {
		fprintf(stderr, "%s: ditroff push/pop overflow!\n", prog);
		exit(2);
	}
	ditstack[dlevel].hpos = hpos;
	ditstack[dlevel].vpos = vpos;
	ditstack[dlevel].fontsize = fontsize;
	ditstack[dlevel].fontheight = fontheight;
	ditstack[dlevel].fontslant = fontslant;
	ditstack[dlevel].font = font;
	dlevel++;
	printf("\nditpush\n");
}

t_pop()	/* pop to previous state */
{
	FlushShow(1);
	MoveTo();
	DoMove();
	if (dlevel == 0) {
		fprintf(stderr, "%s: ditroff push/pop underflow!\n", prog);
		exit(2);
	}
	dlevel--;
	hpos = ditstack[dlevel].hpos;
	vpos = ditstack[dlevel].vpos;
	fontsize = ditstack[dlevel].fontsize;
	fontheight = ditstack[dlevel].fontheight;
	fontslant = ditstack[dlevel].fontslant;
	font = ditstack[dlevel].font;
	printf("%d s %d xH %d xS %d f\n",
		fontsize, fontheight, fontslant, font);
	startx = savex = hpos;
	savey = vpos;
	PSx = hpos * PSmag;
	PSy = vpos * PSmag;
	printf("%d %d MXY\n", savex, savey);
	movepending = NONE;
	printf("\nditpop\n");
}

t_page(n)	/* do whatever new page functions */
{
	register int i;

	if (output && ++scount >= spage) {
		t_reset('p');
		scount = 0;
	}
	output = 1;
	FlushShow(0);
	if (!firstpage)
		printf("\n%d p", n);
	firstpage = FALSE;
	printf("\n%%%%Page: %d %d\n", n, ++pageno, n);
	for (i = 0; i <= nfonts; i++)
		if (fontdelta[i] != 0)
			sayload(i, fontname[i].name, (char *) 0);
	vpos = 0;
	PSy = 0;
	printf("%d s %d xH %d xS %d f\n",
		fontsize, fontheight, fontslant, font);
	if (nolist == 0)
		return;
	output = 0;
	for (i = 0; i < nolist; i += 2)
		if (n >= olist[i] && n <= olist[i + 1]) {
			output = 1;
			break;
		}
}

t_size(n)	/* convert integer to internal size number*/
	int n;
{
	FlushShow(1);
	if (fontsize != n) {
		fontsize = n;
#ifdef XMOD
		fontheight = n;
#endif
		printf("%d s\n", fontsize);
	}
}

t_charht(n)	/* set character height to n */
	int n;
{
	FlushShow(1);
	if (fontheight != n) {
		fontheight = n;
		printf("%d xH\n", fontheight);
	}
}

t_slant(n)	/* set slant to n */
	int n;
{
	FlushShow(1);
	if (fontslant != n) {
		fontslant = n;
		printf("%d xS\n", fontslant);
	}
}

t_font(s)	/* convert string to internal font number */
	char *s;
{
	int n;

	n = atoi(s);
	return n < 0 || n > nfonts ? 1 : n;
}

t_text(s)	/* print string s as text??? */
	char *s;
{
	fprintf(stderr, "%s: ditroff t <%s> unimplemented!\n", prog, s);
}

t_reset(c)
{
	output = 1;			/* by God */
	if (c == 'p')
		printf("\nxp\n");
	else if (!stopped) {
		printf("\nxs\n");
		stopped = 1;
	}
	fflush(stdout);
}

t_res(res, minh, minv)
	int res, minh, minv;
{
	resolution = res;
	minhoriz = minh;
	minvert = minv;
	printf("%d %d %d xr\n", res, minh, minv);
}

t_trailer()
{
	FlushShow(0);
	printf("\n%d p", pageno);
	printf("\n%%%%Trailer\n");
	printf("xt\n");
}

put1s(s)	/* s is a funny char name */
char *s;
{
	int i;

	if (!output)
		return;
	debugp(("%s ", s));

	/* search for s in the funny char name table */
	for (i = 0; i < nchtab; i++)
		if (strcmp(&chname[chtab[i]], s) == 0)
			break;
	if (i < nchtab)
		put1(i + 128, s);
	else {
		debugp(("not found "));
		putnf(0, s);
	}
}

#define needsescape(c) ((c) == '\\' || (c) == '(' || (c) == ')')

put1(c, s)	/* output char c */
	int c;
	char *s;
{
	char *pw;
	register char *p;
	register int i, k;
	register int cc;
	int ofont, code;
	int psinfo, pswid, tw;

	if (!output)
		return;
	if (c == ' ') {
		thisw = 0;
		FlushShow(0);
		return;
	}
	if (c < ' ') {
		debugp(("non-exist 0%o\n", c));
		return;
	}
	c -= 32;	/* offset char code */
	k = ofont = pfont = font;
	if (onspecial)
		pfont = prevfont;
	if ((i = fitab[pfont][c] & BMASK) != 0) {	/* char on this font */
		p = codetab[pfont];
		pw = widthtab[pfont];
		if (onspecial) {
			setfont(prevfont);
			thisw = 0;
			onspecial = 0;
		}
	} else if (smnt > 0) {			/* on special (we hope) */
		for (k = smnt; k <= nfonts; k += 1)
			if ((i = fitab[k][c] & BMASK) != 0) {
				p = codetab[k];
				pw = widthtab[k];
				prevfont = pfont;
				if (onspecial && k == specfont)
					break;
				setfont(k);
				thisw = 0;
				onspecial = 1;
				specfont = k;
				break;
			}
	}
	if (i == 0 || k > nfonts || (code = p[i] & BMASK) == 0) {
		debugp(("not found 0%o\n", c + 32));
		putnf(c + 32, s);
		return;
	}
	/*
	 * when we get here,
	 *  c == biased character code
	 *	k == font number
	 *  i == index into codetab and widthtab for this character
	 *  p == codetab for this font
	 *  pw == width tab for this font
	 *  code == character code for this char
	 */
	cc = c + 32;
	debugp(((isascii(cc) && isprint(cc)) ? "%c %d\n":"%03o %d\n",
		cc, code));
	psinfo = pswidths[font][code];	/* PS specific char info */
	pswid = psinfo & PSWID;		/* PS character width */
	thisw = pw[i] & BMASK;		/* troff char width */
	tw = thisw = (thisw * fontsize + dev.unitwidth / 2) / dev.unitwidth;

	if (psinfo & ISPSPROC && psinfo != -1) {
		/* character is implemented by a PostScript proc */
		showspecial(s, code, pswid);
		if (pswid > 0)
			PSx += PSscale(pswid * fontsize * dres);
		thisw = 0;
	} else {
		showchar(code);
		if (pswid > 0)
			PSshowlen += PSscale(pswid * fontsize * dres);
	}

	/*
	if (font != ofont) {
		setfont(ofont);
		startx = hpos + tw;
		thisw = 0;
		lastcmd = FNT;
	}
	*/
	debugp(("...width (%d)\n", pw[i] & BMASK));
}

putnf(c, s)	/* note that a character wasnt found */
	int c;
	char *s;
{
	FlushShow(0);
	thisw = 0;
	if (s == NULL || *s == '\0')
		printf("(\%3o)cb\n", c);
	else if (strcmp(s, "\\|") == 0 || strcmp(s, "\\^") == 0 ||
		 strcmp(s, "\\&") == 0)
		return;
	else
		printf("(%s)cb\n", s);
}

t_fp(n, s, si)	/* font position n now contains font s, intname si */
	int n;		/* position */
	char *s;	/* font (ditname) */
	char *si;	/* font (intname = number) */
{
	fontname[n].name = s;
	fontname[n].number = atoi(si);
}

setfont(n)	/* set font to n */
	int n;
{
	FlushShow(1);
	if (n < 0 || n > NFONT)
		fprintf(stderr, "%s: illegal font %d\n", prog, n);
	if (font != n) {
		font = n;
		printf("%d f\n", font);
	}
	onspecial = 0;
}

drawline(dx, dy)	/* draw line from here to dx, dy */
	int dx, dy;
{
	FlushShow(0);
	MoveTo();
	DoMove();
	printf("%d %d Dl\n", dx, dy);
	hpos += dx;
	PSx = hpos * PSmag;
	vpos += dy;
	PSy = vpos * PSmag;
}

drawcurve(line)
	char *line;
{
	FlushShow(0);
	MoveTo();
	DoMove();
	getpoints(line + 1);
	/* hpos and vpos won't be changed by curve drawing code */
	hpos = x[numpoints];
	vpos = y[numpoints];
	switch (*line) {
	case 'g':
		IS_Initialize();
		IS_Convert();
		break;
	case '~':
		BS_Initialize();
		BS_Convert();
		break;
	case 'z':
		BZ_Offsets();
		BZ_Convert();
		break;
	}
	printf("Dstroke\n");
}

drawpoly(line)
	char *line;
{
	int stipple;
	register i;
	register char *p;
	int minx, miny, maxx, maxy;

	FlushShow(0);
	MoveTo();
	DoMove();
	for (p = line + 1; isspace(*p); p++)
		;
	for (stipple = 0; isdigit(*p);
	     stipple = stipple * 10 + *p++ - '0')
		;
	getpoints(p);
	minx = maxx = hpos;
	miny = maxy = vpos;
	for (i = 1; i <= numpoints; i++) {
		printf(" %lg %lg lineto\n", x[i], y[i]);
		if (x[i] > maxx)
			maxx = x[i];
		if (x[i] < minx)
			minx = x[i];
		if (y[i] > maxy)
			maxy = y[i];
		if (y[i] < miny)
			miny = y[i];
	}
	printf("closepath %d %d %d %d %d D%c\n",
	       stipple, minx, miny, maxx, maxy, *line);
	/* XXX, hpos and vpos not changed? */
	PSx = x[numpoints] * PSmag;
	PSy = y[numpoints] * PSmag;
}

getpoints(s)
	register char *s;
{
	int h = hpos, v = vpos;

	numpoints = 0;
	for (;;) {
		int dh, dv, neg;

		numpoints++;
		x[numpoints] = h;
		y[numpoints] = v;
		if (numpoints >= MAXPOINTS - 2)	/* -2 for good measure */
			break;
		for (; isspace(*s); s++)
			;
		if (neg = *s == '-')
			s++;
		if (!isdigit(*s))
			break;
		for (dh = 0; isdigit(*s); dh = dh * 10 + *s++ - '0')
			;
		if (neg)
			dh = - dh;
		for (; isspace(*s); s++)
			;
		if (neg = *s == '-')
			s++;
		if (!isdigit(*s))
			break;
		for (dv = 0; isdigit(*s); dv = dv * 10 + *s++ - '0')
			;
		if (neg)
			dv = - dv;
		h += dh;
		v += dv;
	}
}

drawcirc(d)
	int d;
{
	FlushShow(0);
	MoveTo();
	DoMove();
	printf("%d Dc\n", d);
	hpos += d;
	PSx = hpos * PSmag;
}

drawarc(dx1, dy1, dx2, dy2)
	int dx1, dy1, dx2, dy2;
{
	FlushShow(0);
	MoveTo();
	DoMove();
	printf("%d %d %d %d Da\n", dx1, dy1, dx2, dy2);
	hpos += dx1 + dx2;
	PSx = hpos * PSmag;
	vpos += dy1 + dy2;
	PSy = vpos * PSmag;
}

drawellip(a, b)
	int a, b;
{
	FlushShow(0);
	MoveTo();
	DoMove();
	printf("%d %d De\n", a, b);
}

hmot(a)	/* relative horizontal motion */
	int a;
{
	register int aa;

	aa = abs(a);
	if (aa < 8 || aa > 10 * thisw || a >= 100 ||
	    thisw != 0 && abs(thisw - a) > 4)
		FlushShow(1);
	hpos += a;
	if (lastcmd != CPUT)
		startx = hpos;
}

hgoto(a) /* absolute horizontal motion */
	int a;
{
	FlushShow(1);
	startx = hpos = a;
	thisw = 0;
}

vmot(a) /* relative vertical motion */
	int a;
{
	FlushShow(1);
	vpos += a;
	thisw = 0;
}

vgoto(a) /* absolute vertical motion */
	int a;
{
	FlushShow(1);
	vpos = a;
	thisw = 0;
}

showspecial(s, cc, wid)
	char *s;
	int cc;
	int wid;
{
	char *sp;

	FlushShow(0);
	MoveTo();
	DoMove();
	putchar('(');
	for (sp = s; *sp != '\0'; sp++) {
		if (needsescape(*sp))
			putchar('\\');
		putchar(*sp);
	}
	printf(")%d %d oc\n", cc, wid);
}

showchar(c)
	int c;
{
	if (showind == 0)
		MoveTo();
	else if (vpos * PSmag != PSy) {
		FlushShow(0);
		MoveTo();
	}
	if (showind >= SHOWSIZE)
		FlushShow(0);
	if (isascii(c) && isprint(c))
		switch (c) {
		case '\\':
		case '(':
		case ')':
			showbuf[showind++] = '\\';
			/* fall through */
		default:
			showbuf[showind++] = c;
		}
	else {
		showbuf[showind++] = '\\';
		showbuf[showind++] = ((c >> 6) & 03) + '0';
		showbuf[showind++] = ((c >> 3) & 07) + '0';
		showbuf[showind++] = (c & 07) + '0';
	}
	showbuf[showind] = '\0';
	nshow++;
}

MoveTo()
{
	int x, y;

	x = hpos * PSmag;
	y = vpos * PSmag;
	if (x != PSx) {
		startx = savex = hpos;
		PSx = x;
		movepending |= XMOVE;
	}
	if (y != PSy) {
		savey = vpos;
		PSy = y;
		movepending |= YMOVE;
	}
}

FlushMove()
{
	switch (movepending) {
	case NONE:
		break;
	case XMOVE:
		printf("%d", savex);
		break;
	case YMOVE:
		printf("%d", savey);
		break;
	case XYMOVE:
		printf("%d %d", savex, savey);
		break;
	default:
		fprintf(stderr, "%s: invalid move code %d\n",
			prog, movepending);
		exit(2);
	}
}

char *movecmds[] = { "MX", "MY", "MXY" };

DoMove()
{
	FlushMove();
	if (movepending != NONE) {
		printf(" %s\n", movecmds[movepending - 1]);
		movepending = NONE;
	}
}

char showops[] = "SXYN";

FlushShow(t)
	int t;
{
	long err, tlen;
	float cerror;

	if (showind == 0) {
		thisw = 0;
		return;
	}
	if (movepending != NONE)
		FlushMove();
	tlen = hpos - startx;
	if (lastcmd == CPUT)
		tlen += thisw;
	err = tlen * PSmag - PSshowlen;
	if (nshow != 1 && abs(err) > ErrorTolerance) {
		cerror = (float) err / ((nshow - 1) * PSmag);
#ifdef DEBUG
		fprintf(stderr, "F%d lc %d thisw %d ", t, lastcmd, thisw);
		fprintf(stderr, "x %ld h %ld tn %ld %ld ",
			startx, hpos, tlen*PSmag, PSshowlen);
		fprintf(stderr, "error %d %.4f %s\n", nshow, cerror, showbuf);
		fflush(stderr);
#endif
		printf(" %.4f(%s)A%c\n", cerror, showbuf, showops[movepending]);
	} else
		printf("(%s)%c\n", showbuf, showops[movepending]);
	showind = 0;
	nshow = 0;
	showbuf[showind] = '\0';
	PSx += PSshowlen;
	PSshowlen = 0;
	startx = hpos;
	if (lastcmd == CPUT)
		startx += thisw;
	thisw = 0;
	movepending = NONE;
}

/* The following stolen (with modifications) from ... */
/*
 * This program is part of gr2ps.  It converts Gremlin's curve output to
 * control vertices of Bezier Cubics, as supported by PostScript.
 * Gremlin currently supports three kinds of curves:
 *	(1) cubic interpolated spline with
 *	     i) periodic end condition, if two end points coincide
 *	    ii) natural end condition, otherwise
 *	(2) uniform cubic B-spline with
 *	     i) closed curve (no vertex interpolated), if end vertices coincide
 *	    ii) end vertex interpolation, otherwise
 *	(3) Bezier cubics
 *
 * The basic idea of the conversion algorithm for the first two is
 *	(1) take each curve segment's two end points as Bezier end vertices.
 *	(2) find two intermediate points in the orginal curve segment
 *	    (with u=1/4 and u=1/2, for example).
 *	(3) solve for the two intermediate control vertices.
 * The conversion between Bezier Cubics of Gremlin and that of PostScript
 * is straightforward.
 *
 * Author: Peehong Chen (phc@renoir.berkeley.edu)
 * Date: 9/17/1986
 */
#include <math.h>

#define BezierMax	5
#define BC1		1.0/9		/* coefficient of Bezier conversion */
#define BC2		4*BC1
#define BC3		3*BC2
#define BC4		8*BC2

double Qx, Qy, h[MAXPOINTS], dx[MAXPOINTS],
	dy[MAXPOINTS], d2x[MAXPOINTS], d2y[MAXPOINTS], d3x[MAXPOINTS],
	d3y[MAXPOINTS];

/*
 * This routine converts each segment of a curve, P1, P2, P3, and P4
 * to a set of two intermediate control vertices, V2 and V3, in a Bezier
 * segment, plus a third vertex of the end point P4 (assuming the current
 * position is P1), and then writes a PostScript command "V2 V3 V4 curveto"
 * to the output file.
 * The two intermediate vertices are obtained using
 *    Q(u) = V1 * (1-u)^3 + V2 * 3u(1-u)^2 + V3 * 3(1-u)u^2 + V4 * u^3
 * with u=1/4, and u=1/2,
 *	Q(1/4) = Q2 = (x2, y2)
 *	Q(1/2) = Q3 = (x3, y3)
 *	V1 = P1
 *	V4 = P4
 * and
 *	V2 = (32/9)*Q2 - (4/3)*(Q3 + V1) + (1/9)*V4
 *	V3 = -(32/9)*Q2 + 4*Q3 + V1 - (4/9)*V4
 */
BezierSegment(x1, y1, x2, y2, x3, y3, x4, y4)
	double x1, y1, x2, y2, x3, y3, x4, y4;
{
	double V2x, V2y, V3x, V3y;

	V2x = BC4 * x2 - BC3 * (x3 + x1) + BC1 * x4;
	V2y = BC4 * y2 - BC3 * (y3 + y1) + BC1 * y4;
	V3x = -BC4 * x2 + 4 * x3 +  x1 - BC2 * x4;
	V3y = -BC4 * y2 + 4 * y3 +  y1 - BC2 * y4;

	printf(" %lg %lg %lg %lg %lg %lg curveto\n",
		V2x, V2y, V3x, V3y, x4, y4);
	PSx = x4 * PSmag;
	PSy = y4 * PSmag;
} /* end BezierSegment */

/*
 * This routine calculates parameteric values for use in calculating
 * curves.  The values are an approximation of cumulative arc lengths
 * of the curve (uses cord * length).  For additional information,
 * see paper cited below.
 *
 * This is from Gremlin (called Paramaterize in gremlin),
 * with minor modifications (elimination of param list)
 */
IS_Parameterize()
{
	register i, j;
	double t1, t2;
	double u[MAXPOINTS];

	for (i = 1; i <= numpoints; i++) {
		u[i] = 0.0;
		for (j = 1; j < i; j++) {
			t1 = x[j + 1] - x[j];
			t2 = y[j + 1] - y[j];
			u[i] += sqrt(t1 * t1 + t2 * t2);
		}
	}
	for (i = 1; i < numpoints; i++)
		h[i] = u[i + 1] - u[i];
}  /* end IS_Parameterize */

/*
 * This routine solves for the cubic polynomial to fit a spline
 * curve to the the points  specified by the list of values.
 * The curve generated is periodic.  The alogrithms for this
 * curve are from the "Spline Curve Techniques" paper cited below.
 *
 * This is from Gremlin (called PeriodicSpline in gremlin)
 *
 */
IS_PeriodicEnd(h, z, dz, d2z, d3z)
	double h[MAXPOINTS];		/* Parameterizeaterization */
	double z[MAXPOINTS];		/* point list */
	double dz[MAXPOINTS];		/* to return the 1st derivative */
	double d2z[MAXPOINTS];		/* 2nd derivative */
	double d3z[MAXPOINTS];		/* and 3rd derivative */
{
	double a[MAXPOINTS];
	double b[MAXPOINTS];
	double c[MAXPOINTS];
	double d[MAXPOINTS];
	double deltaz[MAXPOINTS];
	double r[MAXPOINTS];
	double s[MAXPOINTS];
	double ftmp;
	register i;

	/* step 1 */
	for (i = 1; i < numpoints; i++)
		if (h[i] != 0)
			deltaz[i] = (z[i + 1] - z[i]) / h[i];
		else
			deltaz[i] = 0;
	h[0] = h[numpoints - 1];
	deltaz[0] = deltaz[numpoints - 1];

	/* step 2 */
	for (i = 1; i < numpoints - 1; i++)
		d[i] = deltaz[i + 1] - deltaz[i];
	d[0] = deltaz[1] - deltaz[0];

	/* step 3a */
	a[1] = 2 * (h[0] + h[1]);
	if (a[1] == 0)
		return (-1);  /* 3 consecutive knots at same point */
	b[1] = d[0];
	c[1] = h[0];

	for (i = 2; i < numpoints - 1; i++) {
		ftmp = h[i - 1];
		a[i] = ftmp + ftmp + h[i] + h[i] - ftmp * ftmp / a[i - 1];
		if (a[i] == 0)
			return (-1);  /* 3 consec knots at same point */
		b[i] = d[i - 1] - ftmp * b[i - 1] / a[i - 1];
		c[i] = - ftmp * c[i - 1]/a[i - 1];
	}

	/* step 3b */
	r[numpoints - 1] = 1;
	s[numpoints - 1] = 0;
	for (i = numpoints - 2; i > 0; i--) {
		r[i] = - (h[i] * r[i + 1] + c[i]) / a[i];
		s[i] = (6 * b[i] - h[i] * s[i + 1]) / a[i];
	}

	/* step 4 */
	d2z[numpoints - 1] = (6 * d[numpoints - 2] - h[0] * s[1]
			- h[numpoints - 1] * s[numpoints - 2])
		/ (h[0] * r[1] + h[numpoints - 1] * r[numpoints - 2]
			+ 2 * (h[numpoints - 2] + h[0]));
	for (i = 1; i < numpoints - 1; i++)
		d2z[i] = r[i] * d2z[numpoints - 1] + s[i];
	d2z[numpoints] = d2z[1];

	/* step 5 */
	for (i = 1; i < numpoints; i++) {
		dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i + 1]) / 6;
		if (h[i] != 0)
			d3z[i] = (d2z[i + 1] - d2z[i]) / h[i];
		else
			d3z[i] = 0;
	}

	return (0);
}  /* end IS_PeriodicEnd */

/*
 * This routine solves for the cubic polynomial to fit a spline
 * curve from the points specified by the list of values.  The alogrithms for
 * this curve are from the "Spline Curve Techniques" paper cited below.
 *
 * This is from Gremlin (called NaturalEndSpline in gremlin)
 */
IS_NaturalEnd(h, z, dz, d2z, d3z)
	double h[MAXPOINTS];		/* parameterization */
	double z[MAXPOINTS];		/* point list */
	double dz[MAXPOINTS];		/* to return the 1st derivative */
	double d2z[MAXPOINTS];		/* 2nd derivative */
	double d3z[MAXPOINTS];		/* and 3rd derivative */
{
	double a[MAXPOINTS];
	double b[MAXPOINTS];
	double d[MAXPOINTS];
	double deltaz[MAXPOINTS];
	double ftmp;
	register i;

	/* step 1 */
	for (i = 1; i < numpoints; i++)
		if (h[i] != 0)
			deltaz[i] = (z[i + 1] - z[i]) / h[i];
		else
			deltaz[i] = 0;
	deltaz[0] = deltaz[numpoints - 1];

	/* step 2 */
	for (i = 1; i < numpoints - 1; i++)
		d[i] = deltaz[i + 1] - deltaz[i];
	d[0] = deltaz[1] - deltaz[0];

	/* step 3 */
	a[0] = 2 * (h[2] + h[1]);
	if (a[0] == 0)		/* 3 consec knots at same point */
		return (-1);
	b[0] = d[1];

	for (i = 1; i < numpoints - 2; i++) {
		ftmp = h[i + 1];
		a[i] = ftmp + ftmp + h[i + 2] + h[i + 2] -
			(ftmp * ftmp) / a[i - 1];
		if (a[i] == 0)		/* 3 consec knots at same point */
			return (-1);
		b[i] = d[i + 1] - ftmp * b[i - 1] / a[i - 1];
	}

	/* step 4 */
	d2z[numpoints] = d2z[1] = 0;
	for (i = numpoints - 1; i > 1; i--)
		d2z[i] = (6 * b[i - 2] - h[i] * d2z[i + 1]) / a[i - 2];

	/* step 5 */
	for (i = 1; i < numpoints; i++) {
		dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i + 1]) / 6;
		if (h[i] != 0)
			d3z[i] = (d2z[i + 1] - d2z[i]) / h[i];
		else
			d3z[i] = 0;
	}

	return (0);
}  /* end IS_NaturalEnd */

/*
 * Use the same algorithm Gremlin uses to interpolate a given
 * set of points, as described in ``Spline Curve Techniques,''
 * by Pattrick Baudelaire, Robert M. Flegal, and Robert F. Sproull,
 * Xerox PARC Tech Report No. 78CSL-059.
 */
IS_Initialize()
{
	IS_Parameterize();

	/* Solve for derivatives of the curve at each point
	   separately for x and y (parametric). */

	if (x[1] == x[numpoints] && y[1] == y[numpoints]) { /* closed curve */
		IS_PeriodicEnd(h, x, dx, d2x, d3x);
		IS_PeriodicEnd(h, y, dy, d2y, d3y);
	} else {
		IS_NaturalEnd(h, x, dx, d2x, d3x);
		IS_NaturalEnd(h, y, dy, d2y, d3y);
	}
}

/*
 * This routine converts cubic interpolatory spline to Bezier control vertices
 */
IS_Convert()
{
	double t, t2, t3, x2, y2, x3, y3;
	register j, j1;

	for (j = 1; j < numpoints; j++) {
		t = .25 * h[j];
		t2 = t * t;
		t3 = t2 * t;
		x2 = x[j] + t * dx[j] + t2 * d2x[j] / 2.0 + t3 * d3x[j] / 6.0;
		y2 = y[j] + t * dy[j] + t2 * d2y[j] / 2.0 + t3 * d3y[j] / 6.0;

		t = 2 * t;
		t2 = t * t;
		t3 = t2 * t;
		x3 = x[j] + t * dx[j] + t2 * d2x[j] / 2.0 + t3 * d3x[j] / 6.0;
		y3 = y[j] + t * dy[j] + t2 * d2y[j] / 2.0 + t3 * d3y[j] / 6.0;

		j1 = j + 1;
		BezierSegment(x[j], y[j], x2, y2, x3, y3, x[j1], y[j1]);
	}
} /* end IS_Convert */

/*
 * This routine computes a point in B-spline segment, given i, and u.
 * Details of this algorithm can be found in the tech. report cited below.
 */
BS_ComputePoint(i, u, xp, yp)
	int i;
	float u;
	double *xp, *yp;
{
	float u2, u3, b_2, b_1, b0, b1;
	register i1, i_2, i_1;

	i1  = i + 1;
	i_1 = i - 1;
	i_2 = i - 2;

	u2 = u * u;
	u3 = u2 * u;
	b_2 = (1 - 3 * u + 3 * u2 - u3) / 6.0;
	b_1 = (4 - 6 * u2 + 3 * u3) / 6.0;
	b0  = (1 + 3 * u + 3 * u2 - 3 * u3) / 6.0;
	b1  = u3 / 6.0;

	*xp = b_2 * x[i_2] + b_1 * x[i_1] + b0 * x[i] + b1 * x[i1];
	*yp = b_2 * y[i_2] + b_1 * y[i_1] + b0 * y[i] + b1 * y[i1];
} /* end BS_ComputePoint */

/*
 * This routine initializes the array of control vertices
 * We consider two end conditions here:
 *   (1) closed curve -- C2 continuation and end vertex not interpolated, i.e.
 *		V[0] = V[n-1], and
 *		V[n+1] = V[2].
 *   (2) open curve -- end vertex interpolation, i.e.
 *		V[0] = 2*V[1] - V[2], and
 *		V[n+1] = 2*V[n] - V[n-1].
 * Details of uniform cubic B-splines, including other end conditions
 * and important properties can be found in Chapters 4-5 of
 * Richard H. Bartels and Brian A. Barsky,
 * "An Introduction to the Use of Splines in Computer Graphics",
 * Tech. Report CS-83-136, Computer Science Division,
 * University of California, Berkeley, 1984.
 */
BS_Initialize()
{
	register n_1, n1;

	n_1 = numpoints - 1;
	n1  = numpoints + 1;
	if (x[1] == x[numpoints] && y[1] == y[numpoints]) { /* closed curve */
		x[0] = x[n_1];				/* V[0] */
		y[0] = y[n_1];
		x[n1] = x[2];				/* V[n+1] */
		y[n1] = y[2];
	} else {				/* end vertex interpolation */
		x[0] = 2 * x[1] - x[2];			/* V[0] */
		y[0] = 2 * y[1] - y[2];
		x[n1] = 2 * x[numpoints] - x[n_1];	/* V[n+1] */
		y[n1] = 2 * y[numpoints] - y[n_1];
	}
} /* end BS_Initialize */

/*
 * This routine converts uniform cubic B-spline to Bezier control vertices
 */
BS_Convert()
{
	double x1, y1, x2, y2, x3, y3, x4, y4;
	register i;

	for (i = 2; i <= numpoints; i++) {
		BS_ComputePoint(i, 0.0, &x1, &y1);
		BS_ComputePoint(i, 0.25, &x2, &y2);
		BS_ComputePoint(i, 0.5, &x3, &y3);
		BS_ComputePoint(i, 1.0, &x4, &y4);
		if (i == 2)
			printf("%lg %lg moveto\n", x1, y1);
		BezierSegment(x1, y1, x2, y2, x3, y3, x4, y4);
	}
} /* end BS_Convert */

/*
 * This routine copies the offset between two consecutive control points
 * into an array.  That is,
 * 	O[i] = (x[i], y[i]) = V[i+1] - V[i],
 * for i=1 to N-1, where N is the number of points given.
 * The starting end point (V[1]) is saved in (Qx, Qy).
 */
BZ_Offsets()
{
	register i;

	/* Assign offsets btwn points to array for convenience of processing */
	Qx = x[1];
	Qy = y[1];
	for (i = 1; i < numpoints; i++) {
		x[i] = x[i + 1] - x[i];
		y[i] = y[i + 1] - y[i];
	}
}

/*
 * This routine contructs paths of piecewise continuous Bezier cubics
 * in PostScript based on the given set of control vertices.
 * Given 2 points, a straight line is drawn.
 * Given 3 points V[1], V[2], and V[3], a Bezier cubic segment
 * of (V[1], (V[1]+V[2])/2, (V[2]+V[3])/2, V[3]) is drawn.
 * In the case when N (N >= 4) points are given, N-2 Bezier segments will
 * be drawn, each of which (for i=1 to N-2) is translated to PostScript as
 *	Q+O[i]/3  Q+(3*O[i]+O[i+1])/6  K+O[i+1]/2  curveto,
 * where
 *	Q is the current point,
 *	K is the continuation offset = Qinitial + Sigma(1, i)(O[i])
 * Note that when i is 1, the initial point
 *	Q = V[1].
 * and when i is N-2, the terminating point
 *	K+O[i+1]/2 = V[N].
 */
BZ_Convert()
{
	register i, i1;
	double x1, y1, x2, y2, x3, y3, Kx, Ky;

	if (numpoints == 2) {
		printf(" %lg %lg rlineto\n", x[1], y[1]);
		PSx += x[1] * PSmag;
		PSy += y[1] * PSmag;
		return;
	}
	if (numpoints == 3) {
		x1 = Qx + x[1];
		y1 = Qy + y[1];
		x2 = x1 + x[2];
		y2 = y1 + y[2];
		printf(" %lg %lg %lg %lg %lg %lg curveto\n",
		       (Qx + x1) / 2.0, (Qy + y1) / 2.0, (x1 + x2) / 2.0,
		       (y1 + y2) / 2.0, x2, y2);
		PSx = x2 * PSmag;
		PSy = y2 * PSmag;
		return;
	}
	/* numpoints >= 4 */
	Kx = Qx + x[1];
	Ky = Qy + y[1];
	x[1] *= 2;
	y[1] *= 2;
	x[numpoints - 1] *= 2;
	y[numpoints - 1] *= 2;
	for (i = 1; i <= numpoints - 2; i++) {
		i1 = i + 1;
		x1 = Qx + x[i] / 3;
		y1 = Qy + y[i] / 3;
		x2 = Qx + (3 * x[i] + x[i1]) / 6;
		y2 = Qy + (3 * y[i] + y[i1]) / 6;
		x3 = Kx + x[i1] / 2;
		y3 = Ky + y[i1] / 2;
		printf(" %lg %lg %lg %lg %lg %lg curveto\n",
			x1, y1, x2, y2, x3, y3);
		Qx = x3;
		Qy = y3;
		Kx += x[i1];
		Ky += y[i1];
	}
	PSx = x3 * PSmag;
	PSy = y3 * PSmag;
} /* end BZ_Convert */
