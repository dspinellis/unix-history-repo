/*
 *	dip
 *	driver for impress/imagen canon laser printer
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
	D~ x y x y ...	wiggly line by x,y then x,y ...
nb a	end of line (information only -- no action needed)
	b = space before line, a = after
p	new page begins -- set v to 0
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
#include	<sys/time.h>
#include	"dev.h"
#include	"canon.h"
#include	"glyph.h"

#define abs(n)  ((n) >= 0 ? (n) : -(n))

#define	NFONT	10
#define	RES	240		/* resolution of canon */

int	xx, yy;
int	inputarea	= 5;	/* input area = 5 * 8k bytes */
int	rotate	= 0;	/* 0 => portrait, 1 => landscape */
int	output	= 0;	/* do we do output at all? */
int	pageno	= -1;	/* output page number */
int	nolist	= 0;	/* output page list if > 0 */
int	olist[20];	/* pairs of page numbers */

struct dev dev;
struct font *fontbase[NFONT+1];
short	*pstab;
int	nsizes	= 1;
int	nfonts;
int	smnt;	/* index of first special font */
int	nchtab;
char	*chname;
short	*chtab;
char	*fitab[NFONT+1];
char	*widthtab[NFONT+1];	/* widtab would be a better name */
char	*codetab[NFONT+1];	/* device codes */

#define	FATAL	1
#define	BMASK	0377
int	dbg	= 0;
int	res	= 240;		/* input assumed computed according to this resolution */
				/* initial value to avoid 0 divide */
FILE	*tf	= NULL;		/* output file pointer */
char	*tempfile;
char	*fontdir	= "/usr/lib/font";
char	*bitdir		= "/usr/src/local/ditroff/troff/devcan";
char	*acctfile	= "/usr/adm/dipacct";
int	acctpages	= 0;
int	copies		= 1;
char	*username	= "???";
char	*getlogin();
extern char devname[];

FILE *fp	= stdin;	/* input file pointer */
extern int DX, DY, maxdots;

main(argc, argv)
char *argv[];
{
	char buf[BUFSIZ];
	char *mktemp();
	int cleanup();

	username = getlogin();
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'c':
			copies = atoi(&argv[1][2]);
			break;
		case 'r':
			rotate = !rotate;
			break;
		case 't':
			tf = stdout;
			break;
		case 'f':
			bitdir = argv[2];
			argv++;
			argc--;
			break;
		case 'o':
			outlist(&argv[1][2]);
			break;
		case 'i':	/* set input area parameter */
			inputarea = atoi(&argv[1][2]);
			if (inputarea < 1)
				inputarea = 1;
			else if (inputarea > 5)
				inputarea = 5;
			break;
		case 'p':	/* pixels of resolution */
			DX = DY = atoi(&argv[1][2]);
			if (DX == 0)
				DX = DY = 1;
			break;
		case 'n':	/* number of dots in object */
			maxdots = atoi(&argv[1][2]);
			if (maxdots <= 0)
				maxdots = 32000;
			break;
		case 'b':
			fprintf(stderr, "It's never busy!");
			exit(0);
			break;
		case 'd':
			dbg = atoi(&argv[1][2]);
			if (dbg == 0) dbg = 1;
			tf = stdout;
			break;
		}
		argc--;
		argv++;
	}

	tempfile = mktemp("/tmp/dipXXXXX");
	if (tf != stdout) {
		if ((tf = fopen(tempfile, "w")) == NULL) {
			error(FATAL, "can't open temporary file %s", tempfile);
		}
		signal(SIGINT, cleanup);
		signal(SIGHUP, cleanup);
		signal(SIGQUIT, cleanup);
	}

	if (argc <= 1)
		conv(stdin);
	else
		while (--argc > 0) {
			if (strcmp(*++argv, "-") == 0)
				fp = stdin;
			else if ((fp = fopen(*argv, "r")) == NULL)
				error(FATAL, "can't open %s", *argv);
			conv(fp);
			fclose(fp);
		}
/*	banner(username); */
	t_wrapup();
	fclose(tf);
/*	sprintf(buf, "ipr -p %d %s 0</dev/null 1>/dev/null 2>&1 &",
		pageno, tempfile);					*/
	buf[0] = '\0';
	if(dbg){fprintf(stderr, "executing %s\n", buf); done();}
	if (tf != stdout) {
		system(buf);
		account();
	}
	done();
}

outlist(s)	/* process list of page numbers to be printed */
char *s;
{
	int n1, n2, i;

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
	if (dbg)
		for (i=0; i<nolist; i += 2)
			printf("%3d %3d\n", olist[i], olist[i+1]);
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
	register int c, k;
	int m, n, i, n1, m1;
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
			fgets(buf, sizeof(buf), fp);
			switch (buf[0]) {
			case 'l':	/* draw a line */
				sscanf(buf+1, "%d %d", &n, &m);
				t_line(n, m, ".");
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
			case 'g':	/* gremlin curve */
			case '~':	/* wiggly line */
				drawwig(buf+1);
			case 't':
			case 's':
				break;
			default:
				error(FATAL, "unknown drawing function %s", buf);
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
			while (getc(fp) != '\n')
				;
			t_newline();
			break;
		case '#':	/* comment */
			while (getc(fp) != '\n')
				;
			break;
		case 'x':	/* device control */
			devcntrl(fp);
			break;
		default:
			error(!FATAL, "unknown input character %o %c", c, c);
			done();
		}
	}
}

devcntrl(fp)	/* interpret device control functions */
FILE *fp;
{
        char str[20], str1[50], buf[50];
	int c, n;

	fscanf(fp, "%s", str);
	switch (str[0]) {	/* crude for now */
	case 'i':	/* initialize */
		fileinit();
		t_init(0);
		break;
	case 'T':	/* device name */
		fscanf(fp, "%s", devname);
		break;
	case 't':	/* trailer */
		t_trailer();
		break;
	case 'p':	/* pause -- can restart */
		t_reset('p');
		break;
	case 's':	/* stop */
		t_reset('s');
		break;
	case 'r':	/* resolution assumed when prepared */
		fscanf(fp, "%d", &res);
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
			break;
}

fileinit()	/* read in font and code files, etc. */
{
	int i, fin, nw;
	char *malloc(), *filebase, *p;
	char temp[60];

	/* open table for device,
	 * read in resolution, size info, font info, etc.
	 * and set params
	 */

	sprintf(temp, "%s/dev%s/DESC.out", fontdir, devname);
	if ((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open tables for %s", temp);
	read(fin, &dev, sizeof(struct dev));
	nfonts = dev.nfonts;
	nsizes = dev.nsizes;
	nchtab = dev.nchtab;
	filebase = malloc(dev.filesize);	/* enough room for whole file */
	read(fin, filebase, dev.filesize);	/* all at once */
	pstab = (short *) filebase;
	chtab = pstab + nsizes + 1;
	chname = (char *) (chtab + dev.nchtab);
	p = chname + dev.lchname;
	for (i = 0; i <= nfonts; i++) {
		fontbase[i] = NULL;
		widthtab[i] = codetab[i] = fitab[i] = NULL;
	}
	close(fin);
}

fontprint(i)	/* debugging print of font i (0,...) */
{
	int j, k, n;
	char *p;

	printf("font %d:\n", i);
	p = (char *) fontbase[i];
	n = fontbase[i]->nwfont & BMASK;
	printf("base=0%o, nchars=%d, spec=%d, name=%s, widtab=0%o, fitab=0%o\n",
		p, n, fontbase[i]->specfont, fontbase[i]->namefont, widthtab[i], fitab[i]);
	printf("widths:\n");
	for (j=0; j <= n; j++) {
		printf(" %2d", widthtab[i][j] & BMASK);
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

loadfont(n, s, s1)	/* load font info for font s on position n (0...) */
int n;
char *s, *s1;
{
	char temp[60];
	int fin, nw, norig;

	if (n < 0 || n > NFONT)
		error(FATAL, "illegal fp command %d %s", n, s);
	if (fontbase[n] != NULL && strcmp(s, fontbase[n]->namefont) == 0)
		return;
	if (s1 == NULL || s1[0] == '\0')
		sprintf(temp, "%s/dev%s/%s.out", fontdir, devname, s);
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
	if (smnt == 0 && fontbase[n]->specfont == 1)
		smnt = n;
	nw = fontbase[n]->nwfont & BMASK;
	widthtab[n] = (char *) fontbase[n] + sizeof(struct font);
	codetab[n] = (char *) widthtab[n] + 2 * nw;
	fitab[n] = (char *) widthtab[n] + 3 * nw;
	t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
	if (dbg > 1) fontprint(n);
}

cleanup()
{
	unlink(tempfile);
	exit(1);
}

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "dip: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		cleanup();
}


/*
	Here beginneth all the stuff that really depends
	on the canon (we hope).
*/

#define	SLOP	1	/* how much positioning error is allowed? */
#define	MAXX	(8*RES + RES/2)		/* 8-1/2 inches? */
#define	MAXY	(11 * RES)
#define	WIDTH	8
#define	LOGWID	3
#define	K	* 1024	/* clever, so watch out */

char	devname[20]	= "ip";

int	nglyph		= 0;	/* number of glyphs loaded */
int	totglyph	= 0;	/* total space used by glyphs sent down */
int	maxglyph	= 28 K;	/* maximum space for glyphs */

#define	oput(c)	if (output) xychar(c); else;

/* input coordinate system: */

int	size	= 1;
int	font	= 1;		/* current font */
int	hpos;		/* horizontal position where we are supposed to be next (left = 0) */
int	vpos;		/* current vertical position (down positive) */
int	lastw	= 0;	/* width of last input character */
int	DX	= 10;	/* step size in x for drawing */
int	DY	= 10;	/* step size in y for drawing */

/* canon coordinate system: */

int	lastsize	= -1;
int	lastfont	= -1;
int	lastx		= -1;
int	lasty		= -1;
int	lastfam		= -1;

int	drawdot	= '.';	/* draw with this character */
int	drawsize = 1;	/* shrink by this factor when drawing */

t_init(reinit)	/* initialize device */
int reinit;
{
	int i;

	if (! reinit) {
		for (i = 0; i < nchtab; i++)
			if (strcmp(&chname[chtab[i]], "l.") == 0)
				break;
		if (i < nchtab) {
			drawdot = i + 128;
			drawsize = 1;
		} else {
			drawdot = '.';
			drawsize = 2;	/* half size */
		}

		/* some Imagen-specific junk: */
		fprintf(tf, "%1d", inputarea);	/* their kludge for setting */
						/* input area to x * 8k */
		maxglyph = (68 -  inputarea - 4) K;
						/* glyph area = 68K - input */
		fprintf(tf, " %s\n\0", username);	/* terminated string */
		fprintf(tf, "%8.8s", "d_ip1/24");	/* padding 8 bytes */
						     /* ignored but needed */
	}
	hpos = vpos = 0;
	setsize(t_size(10));	/* start somewhere */
}

t_line(n, m, s)
	int n, m;
	char *s;
{
	if (m == 0) {	/* horizontal rule needed */
		if (n > 0) {
			t_rule(n, 2);
			hmot(n);	/* finish at the end */
		} else {
			hmot(n);
			t_rule(-n, 2);
		}
	} else if (n == 0) {	/* vertical rule */
		if (m > 0) {
			vmot(m);	/* finish at the end */
			t_rule(2, m);
		} else {
			t_rule(2, -m);
			vmot(m);
		}
	} else {
		drawline(n, m, s);
	}
}

t_page(pg)	/* do whatever new page functions */
{
	register int i, j, n;
	register unsigned char *p;
	static int firstpage = 1;

	pageno = pg;
	if(dbg)fprintf(stderr, "t_page %d, output=%d\n", pg, output);
	if (output != 0) {
		/* beginning of first page, or */
		/* have just printed something, and seen p<n> for next one */
		/* ought to read in entire page, select needed glyphs */
		putc(AEND, tf);
		firstpage = 0;
	}
	output = in_olist(pg);
	if (output) {
		if (totglyph >= maxglyph) {
			clearglyphs();
			totglyph = 0;
		}
		acctpages++;
	}
	lastx = lasty = -1;
	t_init(1);
}

t_newline()	/* do whatever for the end of a line */
{
	hpos = 0;
}

t_size(n)	/* convert integer to internal size number*/
int n;
{
	int i;

	if (n <= pstab[0])
		return(1);
	else if (n >= pstab[nsizes-1])
		return(nsizes);
	for (i = 0; n > pstab[i]; i++)
		;
	return(i+1);
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

t_reset(c)
{
	int n;

	if (output)
		acctpages++;
}

t_wrapup()
{
	putc(AEND, tf);
	putc(AEOF, tf);
}

account()	/* record paper use */
{
/*					HIDE THIS!!!
	FILE *f = NULL;

	if (tf == stdout)
		return;
	f = fopen(acctfile, "a");
	if (f != NULL) {
		if (username == NULL)
			username = "???";
		fprintf(f, "%4d %s\n", acctpages, username);
	}
*/
}

banner(s)
	char *s;
{
	long t, time();
	char *ctime();

	time(&t);
	putc(AEND, tf); /* clean up previous page */
	setsize(16);
	loadfont(1, "CW", "");
	lastx = lasty = -1;
	vgoto(1500);
	hgoto(500);
	while (*s) {
		put1(*s++);
		hmot(128);
	}
	hmot(3*128);
	put1(' ');
	t_rule(960, 24);
	vgoto(2500);
	hgoto(2000);
	s = ctime(&t);
	while (*s) {
		put1(*s++);
		hmot(128);
	}
}

t_rule(w, h)
{
	hvflush();
	putc(ABRULE, tf);
	putint(w, tf);
	putint(h, tf);
	putint(-h, tf);
}


t_trailer()
{
}

hgoto(n)
{
	hpos = n;	/* this is where we want to be */
			/* before printing a character, */
			/* have to make sure it's true */
}

hmot(n)	/* generate n units of horizontal motion */
int n;
{
	hpos += n;
}

vgoto(n)
{
	vpos = n;
}

vmot(n)	/* generate n units of vertical motion */
int n;
{
	vgoto(vpos + n);	/* ignores rounding */
}

put1s(s)	/* s is a funny char name */
	register char *s;
{
	static int i = 0;

	if (!output)
		return;
	if (dbg) printf("%s ", s);
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
	char *pw;
	register char *p;
	register int i, j, k;
	int ofont, code, w;

	if (!output)
		return;
	c -= 32;
	if (c <= 0) {
		if (dbg) printf("non-exist 0%o\n", c+32);
		return;
	}
	k = ofont = font;
	i = fitab[font][c] & BMASK;
	if (i != 0) {	/* it's on this font */
		p = codetab[font];
		pw = widthtab[font];
	} else if (smnt > 0) {		/* on special (we hope) */
		for (k=smnt, j=0; j <= nfonts; j++, k = (k+1) % (nfonts+1))
			if ((i = fitab[k][c] & BMASK) != 0) {
				p = codetab[k];
				pw = widthtab[k];
				setfont(k);
				break;
			}
	}
	if (i == 0 || (code = p[i] & BMASK) == 0 || k > nfonts) {
		if (dbg) printf("not found 0%o\n", c+32);
		return;
	}
	lastw = pw[i] & 077;
	lastw = (lastw * pstab[size-1] + dev.unitwidth/2) / dev.unitwidth;
	if (dbg) {
		if (isprint(c+32))
			printf("%c %d\n", c+32, code);
		else
			printf("%03o %d\n", c+32, code);
	} else
		oput(code);
	if (font != ofont)
		setfont(ofont);
}

setsize(n)	/* set point size to n (internal) */
int n;
{
	size = n;
}

/* font position info: */

struct {
	char *name;
	int number;
} fontname[NFONT+1];

t_fp(n, s, si)	/* font position n now contains font s, intname si */
int n;
char *s, *si;
{
	fontname[n].name = s;
	fontname[n].number = atoi(si);
}

setfont(n)	/* set font to n */
int n;
{
	if (!output)
		return;
	if (n < 0 || n > NFONT)
		error(FATAL, "illegal font %d", n);
	font = n;
}

done()
{
	exit(0);
}

/*
	The following things manage raster font information.
	The big problem is mapping desired font + size into
	available font + size.  For now, a file RASTERLIST
	contains entries like
		R 6 8 10 14 999
		I 8 10 12 999
		...
	This data is used to create an array "fontdata" that
	describes legal fonts and sizes, and pointers to any
	data from files that has actually been loaded.
*/

struct fontdata {
	char	name[4];	/* e.g., "R" or "PA" */
	int	size[10];	/* e.g., 6 8 10 14 0 */
	struct	fontset	*fsp[10];	/* either NULL or block of data */
};

#define	MAXFONT	60	/* no more than this many fonts forever */

struct	fontdata	fontdata[MAXFONT];
int	maxfonts	= 0;	/* how many actually used; set in initfontdata() */

struct	Fontheader	fh;
struct	fontset {
	int	size;
	int	family;
	struct	Charparam *chp;
	unsigned char	*cdp;	/* char data pointer */
	unsigned char	*chused;	/* bit-indexed; 1 if char downloaded */
};

/* A global variable for the current font+size */
struct	fontset	*fs;
int	nfamily		= 0;	/* number of "families" (font+size) */

initfontdata()	/* read RASTERLIST information */
{
	char name[100];
	FILE *fp;
	int i, j, n;

	sprintf(name, "%s/dev%s/RASTERLIST", fontdir, devname);
	if ((fp = fopen(name, "r")) == NULL)
		error(FATAL, "can't open %s", name);
	maxfonts = 0;
	while (fscanf(fp, "%s", fontdata[maxfonts].name) != EOF) {
		i = 0;
		while (fscanf(fp, "%d", &n) != EOF && n < 100) {
			fontdata[maxfonts].size[i] = n;
			fontdata[maxfonts].fsp[i] = NULL;
			i++;
		}
		fontdata[maxfonts].size[i] = 999;
		if (++maxfonts > MAXFONT)
			error(FATAL, "Too many fonts in RASTERLIST");
	}
	fclose(fp);
	if (dbg) {
		fprintf(stderr, "initfontdata():  maxfonts=%d\n", maxfonts);
		for (i = 0; i < maxfonts; i++) {
			fprintf(stderr, "%.4s ", fontdata[i].name);
			for (j = 0; fontdata[i].size[j] < 100; j++)
				fprintf(stderr, " %3d", fontdata[i].size[j]);
			fprintf(stderr, "\n");
		}
	}
}

getfontdata(f, s)	/* causes loading of font information if needed */
	char *f;
	int s;
{
	int fd, n, i, j;
	char name[100];
	static int first = 1;

	if (first) {
		initfontdata();
		first = 0;
	}

	for (i = 0; i < maxfonts; i++)
		if (strcmp(f, fontdata[i].name) == 0)
			break;
	if (i >= maxfonts)	/* the requested font wasn't there */
		i = 0;		/* use the first one (probably R) */

	/* find the best approximation to size s */
	for (j = 1; s >= fontdata[i].size[j]; j++)
		;
	j--;

	/* open file if necessary */
	if (fontdata[i].fsp[j] == NULL) {
		fs = (struct fontset *) malloc(sizeof(struct fontset));
		fontdata[i].fsp[j] = fs;
		fs->chp = (struct Charparam *) malloc(256*sizeof(struct Charparam));
		sprintf(name, "%s/%s.%d%s", bitdir,
			f, fontdata[i].size[j], rotate? "r" : "");
		fd = open(name, 0);
		if (fd == -1)
			error(FATAL, "can't open %s", name);
		read(fd, &fh, sizeof(struct Fontheader));
		read(fd, fs->chp, 256*sizeof(struct Charparam));
		fs->size = fontdata[i].size[j];
		fs->family = nfamily;
		nfamily += 2;	/* even-odd leaves room for big fonts */
		fs->cdp = (unsigned char *) malloc(fh.f_size);
		fs->chused = (unsigned char *) malloc(256/8);
		for (n = 0; n < 256/8; n++)
			fs->chused[n] = 0;
		n = read(fd, fs->cdp, fh.f_size);
		close(fd);
	}
	fs = fontdata[i].fsp[j];
}

xychar(c)
	register int c;
{
	register unsigned char *p;
	register struct Charparam *par;
	register int x;
	register int y;
	int i, n, rwid, ht, fam;

	x = hpos;
	y = vpos;

	if (font != lastfont || size != lastsize) {
		getfontdata(fontname[font].name, pstab[size-1]);
		lastsize = size;
		lastfont = font;
	}
	par = fs->chp + c;
	p = fs->cdp + par->c_addr;

	fam = fs->family;
	if (c > 127)
		fam++;
	if (fam != lastfam) {
		putc(AF, tf);
		putc(lastfam = fam, tf);
	}

	/* first cut:  ship each glyph as needed. */
	/* ignore memory use, efficiency, etc. */

	if ( !bit(fs->chused, c) ) {	/* 1st use of this character */
		nglyph++;
		totglyph += glspace(par);
		setbit(fs->chused, c);
		putc(ASGLY, tf);
		putint((fam << 7) | c, tf);
		par->c_width = (lastw * RES) / res;
 		putc(par->c_width, tf);	/* character width */
		putc(par->c_left + par->c_right + 1, tf);
		putc(par->c_left, tf);
  /* this nonsense fixes a bug in output produced by rec.c: */
  /* when up is < 0 (and = 0?) size is one too big */
		rwid = (1 + par->c_left + par->c_right + WIDTH-1) / WIDTH;
		ht = par->c_size / rwid;
		par->c_down = ht - par->c_up;
		putc(par->c_down + par->c_up, tf);
		putc(par->c_up, tf);
		for (i = par->c_size; i--; )
			putc(*p++, tf);
	}

	if (y != lasty) {
		putc(AV, tf);
		putint(y<<1, tf);
		lasty = y;
	}

	if (abs(x-lastx) > 127) {
		putc(AH, tf);
		putint(x<<1, tf);
		lastx = x + par->c_width;
	} else if (abs(x-lastx) > SLOP) {
		putc(AM, tf);
		putc(x-lastx, tf);
		putc(AM, tf);
		lastx = x + par->c_width;
	} else {
		lastx += par->c_width;
	}

	if (c <= 127)
		putc(c, tf);	/* fails if c > 127, probably disastrously */
	else
		putc(c-128, tf);
}

hvflush()	/* force position recorded in hpos,vpos */
{
	register int x;
	register int y;

	x = hpos;
	y = vpos;

	if (y != lasty) {
		putc(AV, tf);
		putint(y<<1, tf);
		lasty = y;
	}
	if (abs(x-lastx) > 127) {
		putc(AH, tf);
		putint(x<<1, tf);
		lastx = x;
	} else if (abs(x-lastx) > SLOP) {
		putc(AM, tf);
		putc(x-lastx, tf);
		putc(AM, tf);
		lastx = x;
	}
}

glspace(par)
	struct Charparam *par;
{
	int n;

	/* works only for small glyphs right now */

	n = 12
	  + ((par->c_left+par->c_right+1+15)/16 ) * (par->c_up+par->c_down)
	  + 2;
	return n;
}

clearglyphs()	/* remove "used" bits from all glyphs */
		/* delete all families */
		/* very conservative policy */
{
	int i, j, k;
	struct fontset *f;

	if (tf == stdout) fprintf(stderr, "clear %d glyphs (%d/%d) on page %d\n",
		nglyph, totglyph, maxglyph, pageno);
	for (i = 0; i < maxfonts; i++)
		for (j = 0; fontdata[i].size[j] < 999; j++) {
			f = fontdata[i].fsp[j];
			if (f != NULL) {
				putc(ADELF, tf);
				putc(f->family, tf);
				for (k = 0; k < 256/8; k++)
					f->chused[k] = 0;
			}
		}
}

bit(p, n)	/* return n-th bit of p[] */
char *p;
int n;
{
	return (p[n/8] >> (7 - n%8)) & 01;
}

setbit(p, n)	/* set bit n of p[] */
char *p;
int n;
{
	p[n/8] |= 01 << (7 - n%8);
}

putint(n, f)
int n;
FILE *f;
{
	putc(n >> 8, f);
	putc(n & 0377, f);
}
