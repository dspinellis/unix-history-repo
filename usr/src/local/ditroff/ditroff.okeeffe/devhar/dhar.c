#ifndef lint
static char *sccsid = "@(#)dhar.c	1.4	CWI 1.4	%G%";
#endif
/*
 * Drive the Harris 7500 tyepsetter
 *		    75XX
 * Other machines of that harris serie will probably run as well with this
 *
 * Author: jaap akkerhuis, Oc 1982, Mathematisch Cetrum.
 *
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
w	paddable words space -- no action needed
nb a	end of line (information only -- no action needed)
	b = space before line, a = after
p	new page begins -- set v to 0
tstring print string as plain text
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
/*
 * MC:jna
 * The output language signs { and } are not described
 *
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<signal.h>

#include "../dev.h"
#define	NFONT	10

int	output	= 0;	/* do we do output at all? */
int	nolist	= 0;	/* output page list if > 0 */
int	olist[20];	/* pairs of page numbers */
int	spage	= 9999;	/* stop every spage pages */
int	scount	= 0;

struct	dev	dev;
struct Font *fontbase[NFONT+1];
short	*pstab;
int	nsizes;
int	nfonts;
int	smnt;	/* index of first special font */
int	nchtab;
char	*chname;
short	*chtab;
char	*fitab[NFONT+1];
char	*widthtab[NFONT+1];	/* widtab would be a better name */
char	*codetab[NFONT+1];	/* device codes */
#if	pdp
#define	tosh(a,b)	(a)[2*(b)] & BMASK | ((a)[2*(b) + 1] & BMASK) << BYTE
typedef	char f_code;
f_code	*fonttab[2*(NFONT+1)];	/*MC:jna optional fontcodes */
#endif pdp
#if	tahoe || sun
#define	tosh(a,b)	(a)[2*(b) + 1] & BMASK | ((a)[2*(b)] & BMASK) << BYTE
typedef	char f_code;
f_code	*fonttab[2*(NFONT+1)];	/*MC:jna optional fontcodes */
#endif tahoe || sun
#if	vax
#define	tosh(a,b)	(a)[b]
typedef	short f_code;
f_code	*fonttab[NFONT+1];	/*MC:jna optional fontcodes */
#endif vax

#define	FATAL	1
#define	BMASK	0377
#define BYTE	8
int	dbg	= 0;
int	eflag;
int	cflag;
int	res;		/* input assumed computed according to this resolution */
int	tf = 0;		/* output file will be har.in or standout */
char	*fontdir	= "/usr/local/lib/ditroff/font";
extern char devname[];

#define	abs(n)	((n) >= 0 ? (n) : -(n))

int	font	= 1;	/* current font */
int	hpos;		/* horizontal position where we are supposed to be next (left = 0) */
int	lastw;		/*  width of last printed char, (for t_text()) */
int	vpos;		/* current vertical position (down positive) */
int	horig;		/* h origin of current block; hpos rel to this */
int	vorig;
int	htrue	= 0;
int	vtrue	= 0;
int	DX	= 4;	/* step size in x for drawing */
int	DY	= 4;	/* step size in y for drawing */
int	drawdot	= '.';	/* draw with this character */
int	drawsize = 1;	/* shrink by this factor when drawing */
			/* drawsize will be set in t_init as well! */

main(argc, argv)
char *argv[];
{
	FILE *fp;
	int i;
	int done();
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'f':
		case 'F':
			fontdir = argv[2];
			argv++;
			argc--;
			break;
		case 't':
			tf = 1;	/* stdout */
			break;
		case 'o':
			outlist(&argv[1][2]);
			break;
		case 'd':
			dbg = atoi(&argv[1][2]);
			if (dbg == 0) dbg = 1;
			break;
		case 's':
			spage = atoi(&argv[1][2]);
			if (spage <= 0)
				spage = 9999;
			break;
		case 'e':
			eflag++;
			break;
		case 'c':
			cflag++;
			break;
		}
		argc--;
		argv++;
	}
				/*
				 * Stop every 4 pages to prevent the
				 * Harris to Cut the paper every 6 feet,
				 * wat will likely to be in the middle of
				 * a page. Every for page is proved to be
				 * reasonable.
				 */
	if (spage == 0 || 9999)
		spage = 4;

	if (signal(SIGINT, done) == SIG_IGN) {
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	} else {
		signal(SIGQUIT, done);
		signal(SIGHUP, done);
	}
	signal(SIGTERM, done);
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
	account();
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
		case '{':	/* push down current environment */
			t_push();
			break;
		case '}':
			t_pop();
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
		case 't':	/* straight text */
			fgets(buf, sizeof(buf), fp);
			t_text(buf);
			break;
		case 'D':	/* draw function */
			fgets(buf, sizeof(buf), fp);
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
			case '~':	/* wiggly line */
				drawwig(buf+1);
				break;
			default:
				error(FATAL, "unknown drawing function %s\n", buf);
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
			error(!FATAL, "unknown input character %o %c\n", c, c);
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
	/* these don't belong here... */
	case 'H':	/* char height */
		fscanf(fp, "%d", &n);
		t_charht(t_size(n));
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
	/* read in resolution, size info, font info, etc.
	/* and set params
	*/
	sprintf(temp, "%s/dev%s/DESC.out", fontdir, devname);
	if ((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open tables for %s\n", temp);
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
	for (i = 1; i <= nfonts; i++) {
		fontbase[i] = (struct Font *) p;
		nw = *p & BMASK;	/* 1st thing is width count */
		if (smnt == 0 && fontbase[i]->specfont == 1)
			smnt = i;	/* first special font */
		p += sizeof(struct Font);	/* that's what's on the beginning */
		widthtab[i] = p;
		codetab[i] = p + 2 * nw;
		fitab[i] = p + 3 * nw;
		p += 3 * nw + dev.nchtab + 128 - 32;
		if(fontbase[i]->fonttab == 1) {	/*MC:jna There is a fonttable */
			fonttab[i] = (f_code *)p;	/*MC:jna get it */
			p += nw * sizeof( short );	/* and skip it */
		}
		t_fp(i, fontbase[i]->namefont, fontbase[i]->intname);
		if(dbg > 2) fontprint(i);
	}
	/*MC:jna
	 *
	 * Make space for the font cache for NCH characters
	 * also reserve space for fonttable, if any is to come
         *
	 */
	fontbase[0] = (struct Font *) malloc(3*255 + dev.nchtab + (128-32) + sizeof (struct Font) + 255 * sizeof( short));
	widthtab[0] = (char *) fontbase[0] + sizeof (struct Font);
	fontbase[0]->nwfont = 255;
	fontbase[0]->fonttab = 2;	/* there is room for a fonttable! */
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
	if( fontbase[i]->fonttab == 1)
		printf("base fonttab=0%o\n", fonttab[i]);
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
	if(fontbase[i]->fonttab == 1) {
		printf("\nfonttab:\n");
		for (j=0; j <= n; j++) {
			printf(" %d", fonttab[i][j] );
			if (j % 20 == 19) printf("\n");
		}
	}
	printf("\n");
}

loadfont(n, s, s1)	/* load font info for font s on position n (0...) */
int n;
char *s, *s1;
{
	char temp[60];
	int fin, nw, norig, forig;
	char *p;

	if (n < 0 || n > NFONT)
		error(FATAL, "illegal fp command %d %s", n, s);
	if (strcmp(s, fontbase[n]->namefont) == 0)
		return;
	if (s1 == NULL || s1[0] == '\0')
		sprintf(temp, "%s/dev%s/%s.out", fontdir, devname, s);
	else
		sprintf(temp, "%s/%s.out", s1, s);
	if ((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open font table %s", temp);
	norig = fontbase[n]->nwfont & BMASK;
	forig = fontbase[n]->fonttab;
if(dbg > 3)
	printf("nworig, %d, fonttaborig %d\n", norig, forig);
	/*
	 *MC:jna norig is the original amount of chars in
	 * the (premounted) font)
	 *
	 * first geuss there is no fonttab
	 */
	read(fin, fontbase[n], 3*norig + nchtab+128-32 + sizeof(struct Font));
	if ((fontbase[n]->nwfont & BMASK) > norig || (forig == 0 && fontbase[n]->fonttab == 1))
		error(FATAL, "Font %s too big for position %d\n", s, n);
		/*
		 *MC:jna This means it is wise to make the default mounted
		 * fonts larger then any other mounttable fonts.
		 * And because of the kludge with the fonttable,
		 * Make sure that they all contain fonttables!
		 * It will make your life easier.
		 */
	nw = fontbase[n]->nwfont & BMASK;
if(dbg > 3)
	printf("nw %d\n", nw);
	if(fontbase[n]->fonttab == 1) {
		lseek(fin, 0L, 0);
		read(fin, fontbase[n], 3*norig + nchtab+128-32 + nw*sizeof(short) + sizeof(struct Font));
		/*
		 * There turned out to be a fonttab, so we have to read it in
		 *MC:jna a bit stupid, but the laziest way (for me)
		 */
	}
	close(fin);
	widthtab[n] = (char *) fontbase[n] + sizeof(struct Font);
	codetab[n] = (char *) widthtab[n] + 2 * nw;
	fitab[n] = (char *) widthtab[n] + 3 * nw;
	if(fontbase[n]->fonttab == 1)
		fonttab[n] = (f_code *) (widthtab[n] + 3*nw + nchtab+128-32);
	t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
	fontbase[n]->nwfont = norig;	/* so can later use full original size */
	if(fontbase[n]->fonttab == 0 && forig != 0)
		fontbase[n]->fonttab = 2;
					/* so we signal that there is place
					 * for a fonttab! */

	if (dbg > 2) fontprint(n);
}

done()
{
	t_reset('s');
	exit(0);
}

extern int ex();

/*VARARGS*/
error(f, s, a1, a2, a3, a4, a5, a6, a7)
int f;
{
	fprintf(stderr, "dhar: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	fflush(stderr);
	if (f) {
		ex();
		exit(1);
	}
}

/******************************************************************************
 ******************************************************************************
 *
 * Here begins the stuff that really depends on the harris
 *
 * For the time being, no use is made of the ruling functions of the Harris
 *
 ******************************************************************************
 ******************************************************************************
 */

/*
 * The basic idea is to delay the output as long as possible
 * until you really have to.
 * Until that time we just keep a machine status.
 *
 */

#include "hcodes.h"

char	devname[20] = "har";
int	fcut;
int	nocutting;
unsigned	short	papuse;
char	harcode;

t_init(reinit)	/* initialize device */
int reinit;
{
	register int i;
	extern int size;

	hpos = vpos = 0;

	if( strcmp( devname, "har") != NULL )
		error(FATAL, "This input is not for the harris");

	if (!tf)
		if ( ( tf = creat("@har.in", 0664)) < 0)
			error(FATAL, "Cannot create outputfile");
	
	/* if there is a drawing character, use it */
	for ( i = 0; i < nchtab; i++)
		if (strcmp(&chname[chtab[i]], "l.") == 0)
			break;
	if ( i < nchtab) {
		drawdot = i + 128;
		drawsize = 1;
	} else {
		drawdot = '.';
		drawsize = 3; 	/* 1/3 size */
	}

	output = 1;

	oput(VMV); oput(0); oput(0);
				/* See Harris Manual appendix D */
	oput(HPO);oput(0);oput(0);

		/* some initial size */
	size = 10;
	putsize();
	putfont(999);
	oput(STA);oput(0);oput(0360);

	if( eflag ) {
		operator("Translating");
		oput(EST);	/* enable slave Translator */
		fprintf(stderr,"Slave code translator enabled\n");
	} else
		operator("dhar started");
	
	oput(OB0);		/* reset oblique */
	oput(NAD);		/* No automatic displacement */
	output = 0;
}

/*
 * The reason of struct state is never explained by bwk
 * but it looks like an stack of environments being pushed and popped
 *
 */

#define	MAXSTATE	5

struct state {
	int	ssize;
	int	sfont;
	int	shpos;
	int	svpos;
	int	shorig;
	int	svorig;
};
struct	state	state[MAXSTATE];
struct	state	*statep = state;

t_push()	/* begin a new block */
{
	extern size;

	error(!FATAL, "Different environment entered!");
	hflush();
	statep->ssize = size;
	statep->sfont = font;
	statep->shorig = horig;
	statep->svorig = vorig;
	statep->shpos = hpos;
	statep->svpos = vpos;
	horig = hpos;
	vorig = vpos;
	hpos = vpos = 0;
	if (statep++ >= state+MAXSTATE)
		error(FATAL, "{ nested too deep");
	hpos = vpos = 0;
}

t_pop()	/* pop to previous state */
{
	extern size;
	if (--statep < state)
		error(FATAL, "extra }");
	size = statep->ssize;
	font = statep->sfont;
	hpos = statep->shpos;
	vpos = statep->svpos;
	horig = statep->shorig;
	vorig = statep->svorig;
}

int	pageno	= 0;

t_page(n)	/* do whatever new page functions */
{
	int i;

	if (output) {
		papuse++;
		/*
		 * accounting in pages, for the time being.
		 * New harprot should do the real accounting
		 */
		if (++scount >= spage) {
			t_reset('p');
			scount = 0;
		}
	}
	vpos = 0;
	output = 1;
	++pageno;
	if (nolist == 0)
		return;	/* no -o specified */
	output = 0;
	for (i = 0; i < nolist; i += 2)
		if (n >= olist[i] && n <= olist[i+1]) {
			output = 1;
			break;
		}
}

t_newline()	/* do whatever for the end of a line */
{
	hpos = 0;	/* because we're now back at the left margin */
}

/*
 * A PSZ command on the Harris will change the horizontal & vertical size
 * A HPZ command will change just the Horizontal size.
 *
 * so size will contain horizontal size, and versize the vertical
 */
int	size;		/* current sizenumber (a legal index in pstab) */
int	horsize;	/* current horizontal size */
int	versize;	/* current vertcal size */
int	vsizeflag;	/* if set, versize differs from size */

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
	versize = pstab[n-1];
	if( versize != horsize )
		vsizeflag = 1;
	putsize();
}

int sltab[]	= {   0,  9,  12,  15, -1};	/* possible slanting factors */
int slctab[]	= { OB0, OB1, OB2, OB3 };	/* slanting codes */
int slant;		/* current general slanting factor (of slant cmd) */
int fslant;		/* slanting factor of current font */

/*
 * current font has to be slanted, the slant will be set to fslant.
 * if the has been a slant command, the slant will be set to "slant",
 * overiding the fslant.
 * if slant is reset to 0, and there fslant != 0, slant will be set to "fslant"
 *
 * fslant will be manupulated by setfont (slanting can be an attribute
 * to a (Harris-)font.
 *
 * There are to many slants in this comment
 */

t_slant(n)	/* do slant cmd */
int n;
{	slant = n;
	setslant(n);
}

setslant(n)	/* set slant to n */
int n;
{	int j;
	static int aslant;	/* the actual slanting factor */

	if( n == aslant)
		return;
	if( n == 0 && fslant) {		/* back to slant of font */
		setslant( fslant );
		return;
	}
	for (j = 0; n > ( aslant = sltab[j]); j++)
		if ( aslant == -1) {
			aslant = sltab[--j];
			break;
		}
	hflush();
	oput( slctab[j] );
	if (dbg)
		printf("slant to %d\n", aslant);
}

slantfont(n)	/* set fontslant */
int n;
{
	fslant = n;
	if(slant)
		return;		/* slant of slanting command
				 * overrides fslant */
	setslant( fslant);	/* set slanting */
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

t_text(s)	/* print string s as text, the real \! implemantation */
char *s;
{
	int c, w;
	char str[100];

	error(!FATAL, "t_text not well implented (yet)!");
	if (!output)
		return;
	while (c = *s++) {
		if (c == '\\') {
			switch (c = *s++) {
			case '\\':
			case 'e':
				put1('\\');
				break;
			case '(':
				str[0] = *s++;
				str[1] = *s++;
				str[2] = '\0';
				put1s(str);
				break;
			}
		} else {
			put1(c);
		}
		hmot(lastw);
		if (dbg) printf("width = %d\n", lastw);
	}
}

t_reset(c)
{
	int n;

	if (output)
		/*
		 papuse++
		 */
		 ;
	switch(c) {
	case 'p':
		cut();	/*
			 * interpret pauses as comment for cutting
			 * the paper
			 */
		if(dbg)
			printf("reset p\n");
		break;
	case 's':
		cut();
		nocutting++;
		if(dbg)
			printf("reset s\n");
		ex();
		break;
	default:
		error(!FATAL, "Unknown reset function");
		break;
	}
}

cut()
{
	if (cflag || nocutting)
		return;
	hflush();
	oput(CUT);
	hpos = 0;
	fcut = 1;
	if (dbg)
		printf("Cut\n");
}

account()	/* record paper use */
{
	/* Don somewhere els */
}

t_trailer()
{
}

hflush()	/* do the actual motion */
{
	if (!output)
		return;
	hor_move( hpos - htrue );
}

hor_move( amount )
int amount;
{	int high, low;

#if	vax || tahoe
	if ( abs(amount) > 0177777)
		error(FATAL, "Impossible escape");
#endif
	if ( amount == 0 && harcode == 0)
		return;		/* really nothing to do */
	if(dbg > 1)
		printf("h_move %d\n", amount);
	low = amount & BMASK;
	high = ( amount >> BYTE) & BMASK;
	/*
	 * if there is a code wating for output,
	 * send that one to be output, plus the movement,
	 * else send the MAB
	 * and the movement
	 */
	oput( harcode ? harcode : MAB);
	harcode = 0;
	oput(high);
	oput(low);
	htrue = hpos;
}


hmot(n)
{
	hpos += n;
}

hgoto(n)
{
	hpos = n;
}

vgoto(n)
{
	vmot(n - vpos);
}

vmot(n)	/* generate n units of vertical motion */
int n;
{
	if (!output)
		return;
	if (n != 0) {
		ver_move( n );
		vpos += n;
	}
}

ver_move( amount )
int amount;
{	int high, low;

#if	vax || tahoe
	if ( abs(amount) > 0177777)
		error(FATAL, "Impossible leading");
#endif
	if(dbg > 1)
		printf("v_move %d\n", amount);
	low = amount & BMASK;
	high = ( amount >> BYTE) & BMASK;
	hflush();
	oput(VMV);
	oput(high);
	oput(low);
}

put1s(s)	/* s is a funny char name */
char *s;
{
	int i;

	if (!output)
		return;
/*
	if(strcmp("ul", s) == 0) {
		set_ul();
		return;
	}
	if(strcmp("ru", s) == 0) {
		set_ru();
		return;
	}
*/
	for (i = 0; i < nchtab; i++)
		if (strcmp(&chname[chtab[i]], s) == 0)
			break;
/*
printf("i+128: %d,s: %s, chname: %s\n", i+128, s, &chname[chtab[i]]);
*/
	if (i < nchtab)
		put1(i + 128);
	else
		if(dbg)
			printf("Special char %s doesn't exist\n", s);
}

/*
 * The Harris doesn'nt have a proper underrule or rule
 *
 * Try to generate one with the RULE command.
 *
 */

#define UL_DOWN	7	/* 7 half decipoints at pointsize 10 */

set_ul()
{	int move;
	int tmp;

	hflush();
	move = UL_DOWN * versize;
	ver_move( move);
	tmp = get_width("ul") / 2;
		/*
		 * we assume that dev.unitwidth is 10, so getwidth
		 * will return the value in half decipoints!
		 */
	set_line(tmp);
	ver_move( -move);
}

#define RU_DOWN	1	/* 2 half decipoints at pointsize 10 */

set_ru()
{
	int tmp, move;

	hflush();
	move = RU_DOWN * versize;
	ver_move( move);
	tmp = get_width("ul") / 2;
	set_line(tmp);
	ver_move( -move);
}

#define HEIGHT	6	/* thickness (decipoints) at pointsize 10 */
#define MIN_VAL	2	/* Minimum value for rule height & length */
#define MAX_H	720	/* Maximum for height */
#define MAX_L	8160	/* Maximum length of the SMC 68 Pica machine */

/*
 * set line of length decipoints.
 */

set_line( length )
int length;
{
	int height;
	char one, two, three, four;

	/*
	printf("Line %d decipoints\n", i);
	*/

	height = (HEIGHT * versize + dev.unitwidth/2) / dev.unitwidth;
	if ( height < MIN_VAL)
		height = MIN_VAL;
	if (height > MAX_H)
		height = MAX_H;
	if (length > MAX_L)
		length = MAX_L;
	if (dbg)
		printf("Line: length %d height %d\n", length, height);

	one = ( height >> BYTE ) | RUL;
	two = height & BMASK;
	three = length >> BYTE;
	four = length & BMASK;
	oput(one); oput(two); oput(three); oput(four);
}

/*
 * get the width of a char, to be used only by set_ul() and set-ru()
 */

int
get_width( s )
char *s;
{
	int c;
	int width;
	int j, i, k, ofont;
	char *pw;

	for (c = 0; c < nchtab; c++)
		if (strcmp(&chname[chtab[c]], s) == 0)
			break;
	if (c < nchtab)
		c += 128-32;
	if (c <= 0 || c >= nchtab + 128-32) {
		if (dbg) printf("non-exist 0%o\n", c+32);
		return;
	}
	k = ofont = font;
	i = fitab[font][c] & BMASK;
	if (i != 0) {	/* it's on this font */
		pw = widthtab[font];
	} else if (smnt > 0) {		/* on special (we hope) */
		for (k=smnt, j=0; j <= nfonts; j++, k = (k+1) % (nfonts+1))
			/*
			 * Look for the character, start at the special font
			 * and search further in a wrap around manner 
			 */ 
			if ((i = fitab[k][c] & BMASK) != 0) {
				pw = widthtab[k];
				setfont(k);
				break;
			}
	}
	if (i == 0 || (width = pw[i] & BMASK) == 0 || k > nfonts) {
		/* device drivers do width & 077, not really necessary */
		if (dbg) {
				printf("Width not found \\(%s\n", s);
		}
		return;
	}
	width = (width * horsize + dev.unitwidth/2) / dev.unitwidth;
	if (font != ofont)
		setfont(ofont);
	return( width);
}

/* font position info: */

struct {
	char *name;
	int number;
} fontname[NFONT+1];

put1(c)	/* output char c */
int c;
{
	char *pw;
	register char *p;
	register int i, k;
	int j, ofont, code;
	short f;

	if (!output)
		return;
	c -= 32;
	if (c <= 0) {
		if (dbg) printf("non-exist 0%o\n", c+32);
		lastw = widthtab[font][0] * pstab[size-1] /dev.unitwidth;
		return;
	}
	k = ofont = font;
	i = fitab[font][c] & BMASK;
	if (i != 0) {	/* it's on this font */
		p = codetab[font];
		pw = widthtab[font];
	} else if (smnt > 0) {		/* on special (we hope) */
		for (k=smnt, j=0; j <= nfonts; j++, k = (k+1) % (nfonts+1))
			/*
			 * Look for the character, start at the special font
			 * and search further in a wrap around manner 
			 */ 
			if ((i = fitab[k][c] & BMASK) != 0) {
				p = codetab[k];
				pw = widthtab[k];
				setfont(k);
				break;
			}
	}
	if (i == 0 || (code = p[i] & BMASK) == 0 || k > nfonts) {
		if (dbg) {
			if (isprint(c+32) && isascii(c+32)) 
				printf("not found %c\n", c+32);
			else
				printf("not found \\(%s\n", &chname[chtab[c -128+32]]);
		}
		return;
	}
	if (fontbase[k]->fonttab == 1)
		f = tosh( fonttab[k], i);
	else
		f = fontname[k].number;
	hflush();
	if (dbg) {
		if (isprint(c+32) && isascii(c+32)) { /* My God! */
			printf("%c %d %d\n", c+32, code, f);
		}
		else
			printf("\\(%s %d %d\n", &chname[chtab[c -128+32]], code, f);
	}
	if(code == 0 || code > 0200) {
		error(FATAL,"Illegal code 0%o found for char %03o\n", code, c+32);
	}
	putcode(code, f);	/* character is < 254 */
	if (font != ofont)	/* char on special font, reset	*/
		setfont(ofont);
	lastw = pw[i] & BMASK;
/*HIRO*/
if( dbg)
	fprintf(stderr,"lastw %d pw[i] %d\n", lastw,pw[i]);
	lastw = (lastw * pstab[size-1] + dev.unitwidth/2) / dev.unitwidth;
}

putcode(code, f)
char code; short f;
{
	static short phfont;

#if	vax || tahoe
	if ( f > 0177777)
		error(FATAL, "Impossible font selected");
#endif

	if( harcode) {	/* if character pending */
		hflush();	/* update position and flush pending char */
	}
	if ( f != phfont ) {
		if(dbg > 1)
			printf("font to %d\n", f);
		putfont(f);
	}
	harcode = code;
	phfont = f;
}

putfont(f)
int f;
{	int high, low;

	low = f & BMASK;
	high = (f >> BYTE ) & BMASK;
	oput(FNT);
	oput(high);
	oput(low);
}

setsize(n)	/* set point size to a true pointsize */
int n;
{

	if (!output)
		return;
	horsize = pstab[n-1];
	vsizeflag = 0;
	size = n;
	putsize();
}

/*
 * Do the actual sizechange(s).
 */

putsize()
{
	if(!vsizeflag) {
		flushchar();
		sizecmd( PSZ, horsize);
	}
	else {
		flushchar();
		sizecmd( PSZ, versize);
		sizecmd( HPZ, horsize);
	}
}

sizecmd( cmd, n)
int	cmd, n;
{
	int i, low, high;
	
	i = 10 * n;
	if(dbg)
		printf("size to %d\n", n);
	if( i > 01777)
		error(FATAL, "Impossible pointsize requested");
	low = i & BMASK;
	high = (i >> BYTE) & BMASK;
	if( high > 03 )
		error(FATAL, "system error in point size cmd");
	oput( cmd | high);
	oput(low);
}

t_fp(n, s, si)	/* font position n now contains font s, intname si */
int n;
char *s, *si;
{
	fontname[n].name = s;
	fontname[n].number = atoi(si);
}

setfont(n)	/* set font to n (internal)*/
int n;
{
	if (!output)
		return;
	if (n < 0 || n > NFONT)
		error(FATAL, "illegal font %d\n", n);
	font = n;
	slantfont(fontbase[n]->slant & BMASK);
}

/*
putint(n)
{
	if (dbg) {
		printf("%02d\n", n);
		return;
	}
	putc(n>>8, tf);
	putc(n, tf);
}
*/
