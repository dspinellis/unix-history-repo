/* 
 * dver.c - Versatec driver for the new troff
 *
 * Authors:	BWK(BELL), VCAT(berkley), and Richard L. Hyde
 *		Many parts where lifted from the above sources.
 * Editor:	Richard L. Hyde
 * 		Dept. of Computer Sciences
 * 		Purdue University
 * Date:	Thu Oct 28 1982
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
w	paddable word space -- no action needed
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
#include	<ctype.h>
#include	<signal.h>
#include "dev.h"
#include <errno.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include "modes.h"

/*
 * Versatec troff driver hacked from vcat and others.
 */

#define	NFONT	25
#define OPENREAD 0
#define RESTART 1
#define ABORT 2

int	output	= 0;	/* do we do output at all? */
int	nolist	= 0;	/* output page list if > 0 */
int	olist[20];	/* pairs of page numbers */
int	spage	= 9999;	/* stop every spage pages */
int	scount	= 0;
int	DX	= 1;
int	DY	= 1;
struct	dev	dev;
struct font *fontbase[NFONT+1];
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

#define	FATAL	1
#undef  BMASK
#define	BMASK	0377
int	dbg	= 0;
int	res;		/* input assumed computed according to this resolution */

FILE	*tf	= NULL;	/* output file */
char	*fontdir	= "/usr/lib/font";
extern char devname[];

/* hpos == hpos */
/* vpos != vpos vpos = vertical postion on the page xpos is rel buf0p */

#define	hmot(n)		hpos += n
#define	hgoto(n)	hpos = n
#define	vmot(n)		vgoto(vpos + n)
int	maxX;
int	size	= -1;	/* this is invalid */
int	font	= -1;	/* current font */
int	hpos;		/* horizontal position where we are supposed to be next (left = 0) */
int	oldh;		/* previous H position */
int	vpos;		/* current vertical position (down positive) */
int	oldv;		/* current pos in 1/4 point units */
int	horig;		/* h origin of current block; hpos rel to this */
int	vorig;
int	drawdot	= '.';	/* draw with this character */
int	drawsize = 2;	/* shrink by this factor when drawing */
/* new
 */

#define DISPATCHSIZE		256	/* must be a power of two */
#define CHARMASK		(DISPATCHSIZE-1)
#define NFONTS			25
#define SPECIALFONT		3
#define DSIZ			((sizeof *dispatch)*DISPATCHSIZE)
#define MAXF			(NFONT + 1)
#define OUTFILE 		1


#define RASTER_LENGTH           1600   /* 100 characters, 16 bits / character */
#define BYTES_PER_LINE		(RASTER_LENGTH/8)
#define NLINES			(11*200 + 10)
#define BUFFER_SIZE		(NLINES*BYTES_PER_LINE)

char	buffer[BUFFER_SIZE];	/* Big line buffers  */
char	*buf0p = &buffer[0];	/* Zero origin in circular buffer  */

char	*calloc();
char	*nalloc();
char	*allpanic();
struct	passwd *getpwuid();

struct	header{
	short	magic;
	unsigned short	size;
	short	maxx;
	short	maxy;
	short	xtnd;
} header;

struct	dispatch{
	unsigned short	addr;
	short	nbytes;
	char	up;
	char	down;
	char	left;
	char	right;
	short	width;
};

struct	fontdes {
	int	fnum;
	int	psize;
	struct	dispatch *disp;
	char	*bits;
} fontdes[NFONTS] = {
	-1,
	-1
};

struct dispatch *dispatch;
/* Accounting assumes roll paper */
#define FFLINES		650
#define EOTLINES	1400
int	cfnum = -1;
int	cpsize = 10;
int	cfont = 1;
char	*bits;
int	nfontnum = -1;
int	fontwanted = 1;
int	npsize = 10;

/* accounting variables */
int lines = 0;		/* line counter */
char *user;		/* user name */

/*
    old
 */
 
main(argc, argv)
char *argv[];
{
	FILE *fp;
	int done();

	idput();

	tf = stdout;
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'f':
		case 'F':
			fontdir = argv[2];
			argv++;
			argc--;
			break;
		case 'o':
			outlist(&argv[1][2]);
			break;
		case 'd':
			dbg = atoi(&argv[1][2]);
			if (dbg == 0) dbg = 1;
			tf = stdout;
			break;
		case 's':
			spage = atoi(&argv[1][2]);
			if (spage <= 0)
				spage = 9999;
			break;
		case 'n':	/* user's name */
			argc--;
			user = *++argv;
			break;
		}
		argc--;
		argv++;
	}

	ioctl(OUTFILE, VSETSTATE, pltmode);

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
	accounting();
	waitchild();
	done();
	exit(0);
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
			fprintf(stderr,"%3d %3d\n", olist[i], olist[i+1]);
}

conv(fp)
register FILE *fp;
{
	register int c, k;
	int m, n, n1, m1;
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
	char	*filebase, *p;
	char	temp[60];

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
	filebase = calloc(1,dev.filesize);	/* enough room for whole file */
	read(fin, filebase, dev.filesize);	/* all at once */
	pstab = (short *) filebase;
	chtab = pstab + nsizes + 1;
	chname = (char *) (chtab + dev.nchtab);
	p = chname + dev.lchname;
	for (i = 1; i <= nfonts; i++) {
		fontbase[i] = (struct font *) p;
		nw = *p & BMASK;	/* 1st thing is width count */
		if (smnt == 0 && fontbase[i]->specfont == 1)
			smnt = i;	/* first special font */
		p += sizeof(struct font);	/* that's what's on the beginning */
		widthtab[i] = p;
		codetab[i] = p + 2 * nw;
		fitab[i] = p + 3 * nw;
		p += 3 * nw + dev.nchtab + 128 - 32;
		t_fp(i, fontbase[i]->namefont, fontbase[i]->intname);
		if(dbg > 1) fontprint(i);
	}
	fontbase[0] = (struct font *) calloc(1,3*255 + dev.nchtab + (128-32) + sizeof (struct font));
	widthtab[0] = (char *) fontbase[0] + sizeof (struct font);
	fontbase[0]->nwfont = 255;
	close(fin);
}

fontprint(i)	/* debugging print of font i (0,...) */
{
	int j, n;
	char *p;

	fprintf(stderr,"font %d:\n", i);
	p = (char *) fontbase[i];
	n = fontbase[i]->nwfont & BMASK;
	fprintf(stderr,"base=0%o, nchars=%d, spec=%d, name=%s, widtab=0%o, fitab=0%o\n",
		p, n, fontbase[i]->specfont, fontbase[i]->namefont, widthtab[i], fitab[i]);
	fprintf(stderr,"widths:\n");
	for (j=0; j <= n; j++) {
		fprintf(stderr," %2d", widthtab[i][j] & BMASK);
		if (j % 20 == 19) fprintf(stderr,"\n");
	}
	fprintf(stderr,"\ncodetab:\n");
	for (j=0; j <= n; j++) {
		fprintf(stderr," %2d", codetab[i][j] & BMASK);
		if (j % 20 == 19) fprintf(stderr,"\n");
	}
	fprintf(stderr,"\nfitab:\n");
	for (j=0; j <= dev.nchtab + 128-32; j++) {
		fprintf(stderr," %2d", fitab[i][j] & BMASK);
		if (j % 20 == 19) fprintf(stderr,"\n");
	}
	fprintf(stderr,"\n");
}

loadfont(n, s, s1)	/* load font info for font s on position n (0...) */
int n;
char *s, *s1;
{
	char temp[60];
	int fin, nw, norig;

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
	read(fin, fontbase[n], 3*norig + nchtab+128-32 + sizeof(struct font));
	if ((fontbase[n]->nwfont & BMASK) > norig)
		error(FATAL, "Font %s too big for position %d\n", s, n);
	close(fin);
	nw = fontbase[n]->nwfont & BMASK;
	widthtab[n] = (char *) fontbase[n] + sizeof(struct font);
	codetab[n] = (char *) widthtab[n] + 2 * nw;
	fitab[n] = (char *) widthtab[n] + 3 * nw;
	t_fp(n, fontbase[n]->namefont, fontbase[n]->intname);
	fontbase[n]->nwfont = norig; /* so can later use full original size */
	if (dbg > 1) fontprint(n);
}

done()
{
	t_reset('s');
	exit(0);
}
/*VARARGS1*/
error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "dver: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		done();
}

/*
	Here beginneth all the stuff that really depends
	on the versatec  (we hope).
*/


char	devname[20]	= "dver";

#define	RES	200

#define	TRAILER	(14 * res)
#define	LMARGIN	0		/* left margin offset */
#define	HMAX	(48 * (res/6))	/* maximum horizontal size = 48 picas */
#define	VMAX	(12 * res)	/* 15 inch page */

int	lastw;	/* last character and width (maybe not used) */

long	paper;		/* paper used */


t_init(reinit)	/* initialize device */
int reinit;
{
	int i;

	hpos = vpos = oldv = oldh = 0;
	/* the above are not true until the code below happens*/
	setsize(t_size(10));	/* start somewhere */
	setfont(1);

	for (i = 0; i < nchtab; i++)/* find the line drawing character */
		if (strcmp(&chname[chtab[i]], "l.") == 0)
			break;
	if (i < nchtab) {
		drawdot = i + 128;
		drawsize = 1;
	} else {
		drawdot = '.';
		drawsize = 2;	/* half size */
	}
}

#define	MAXSTATE	6

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
	if (--statep < state)
		error(FATAL, "extra }");
	size = statep->ssize;
	font = statep->sfont;
	hpos = statep->shpos;
	vpos = statep->svpos;
	horig = statep->shorig;
	vorig = statep->svorig;
}

t_page(n)	/* do whatever new page functions */
{
	int i;


	if (output) {
		if (tf != stdout)
			paper += vpos;
		if (++scount >= spage) {
			t_reset('p');
			scount = 0;
		}
	/* flush out the buffer. Do it in a child proc */
	/* this buffering was added by Bob Brown to stop the versatec */
	/* from smearing while waiting for the next page */
		waitchild();
		if (fork() == 0) {
			slop_lines(maxX);
			ioctl(OUTFILE, VSETSTATE, prtmode);
			if (write(OUTFILE, "\f", 2) != 2)
				exit(RESTART);
			ioctl(OUTFILE, VSETSTATE, pltmode);
			exit(0);
		}
		lines += maxX;
		size = BYTES_PER_LINE * maxX;
		vclear(buf0p, size);
		buf0p = buffer;
	}

	maxX = 0;

	vpos = 0;
	oldv = 0;
	output = 1;
	if (nolist == 0)
		return;	/* no -o specified */
	output = 0;
	for (i = 0; i < nolist; i += 2)
		if (n >= olist[i] && n <= olist[i+1]) {
			output = 1;
			break;
		}
}
waitchild()
{
	int status;
	while ( wait(&status) != -1 )
		if ( ((status>>8) & 0xff ) != 0 )
			exit((status>>8) & 0xff);
}

t_newline()	/* do whatever for the end of a line */
{
	hpos = 0;	/* because we're now back at the left margin */

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
	if(dbg)fprintf(stderr,"can't set height on versatec \n");
}

t_slant(n)	/* set slant to n */
int n;
{
	if(dbg)fprintf(stderr,"can't set slant on versatec yet\n");
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

t_text(s)	/* print string s as text */
char *s;
{
	int c;
	char str[100];

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
		if (dbg) fprintf(stderr,"width = %d\n", lastw);
	}
}

t_reset(c)
{	


	if (output)
		paper += vpos;
	output = 1;
	switch(c){
	case 'p':
		waitchild();
		slop_lines(maxX);
		maxX = 0;
		buf0p = buffer;
		break;
	case 's':
		waitchild();
		slop_lines(maxX);
		t_done();
		break; /* no Return */
	}
}
t_done(){


	/* clean up and get ready to die */
	waitchild();
	ioctl(OUTFILE, VSETSTATE, prtmode);
	if (write(OUTFILE, "\f", 2) != 2)
		exit(RESTART);
}

t_trailer()
{
	vpos = oldv = 0;
	vgoto(TRAILER);
	vpos = oldv = 0;
}

vgoto(n)
{
	if(n < 0){
		/* check to see if n would move use past buf0p */
		fprintf(stderr,"ERROR vgoto past the beginning");
		done();			/* no return */
	}
					/* check for end of page */
	if(n > RES * 11) n -= RES * 11;	/* wrap around on to the top */
	vpos = n;

}


put1s(s)	/* s is a funny char name */
char *s;
{
	int i;

	if (!output)
		return;
	if (dbg) fprintf(stderr,"%s ", s);
	for (i = 0; i < nchtab; i++)
		if (strcmp(&chname[chtab[i]], s) == 0)
			break;
	if (i < nchtab)
		put1(i + 128);
}

put1(c)	/* output char c */
int c;
{
	char *pw;
	register char *p;
	register int i, k;
	int j, ofont, code;

	if (!output)
		return;
	c -= 32;
	if (c <= 0) {
		if (dbg) fprintf(stderr,"non-exist 0%o\n", c + 32);
		lastw = widthtab[font][0] * pstab[size-1] / dev.unitwidth;
		return;
	}
	k = ofont = font;
	i = fitab[font][c] & BMASK;
	if (i != 0) {	/* it's on this font */
		p = codetab[font]; /* get the printing value of ch */
		pw = widthtab[font]; /* get the width */
	} else if (smnt > 0) {		/* on special (we hope) */
		for (k=smnt, j=0; j <= nfonts; j++, k = (k+1) % (nfonts+1)){
			if(fitab[k] == 0)
				continue;
			if ((i = fitab[k][c] & BMASK) != 0) {
				p = codetab[k];
				pw = widthtab[k];
				setfont(k);
				break;
			}
		}
	}
	if (i == 0 || (code = p[i] & BMASK) == 0 || k > nfonts) {
		if (dbg) fprintf(stderr,"not found 0%o\n", c+32);
		return;
	}
	if (dbg) {
		if (isprint(c+32))
			fprintf(stderr,"%c %d\n", c+32, code);
		else
			fprintf(stderr,"%03o %d\n", c+32, code);
	} else
		outc(code);	/* character is < 254 */
	if (font != ofont)
		setfont(ofont);
	lastw = pw[i] & 077;
	lastw = (lastw * pstab[size-1] + dev.unitwidth/2) / dev.unitwidth;
}

/* font position info: */

struct {
	char *name;
	int number;
} fontname[NFONT+1];


setsize(n)	/* set point size to n (internal) */
int n;
{

	if (n == size)
		return;	/* already there */
	if(vloadfont(font,pstab[n-1]) != -1)
		size = n;
}

t_fp(n, s, si)	/* font position n now contains font s, intname si */
int n;
char *s, *si;
{
	register	i;

	fontname[n].name = s;
	fontname[n].number = atoi(si);
	for(i = 0;i < NFONT;i++)/* free the bits of that font */
		if(fontdes[i].fnum == n){
			nfree(fontdes[i].bits);
			fontdes[i].bits = 0;
			fontdes[i].fnum = -1;
		}
}

setfont(n)	/* set font to n */
int n;
{
	if (n < 0 || n > NFONT)
		error(FATAL, "illegal font %d\n", n);
	if(vloadfont(n,pstab[size - 1]) != -1)
		font = n;
}


vloadfont(fnum, fsize)
register int fnum;
register int fsize;
{
	register int i;

	fontwanted = 0;
	if (fnum == cfnum && fsize == cpsize)
		return(0);
	for (i = 0; i < NFONTS; i++)
		if (fontdes[i].fnum == fnum && fontdes[i].psize == fsize) {
			cfnum = fontdes[i].fnum;
			cpsize = fontdes[i].psize;
			dispatch = &fontdes[i].disp[0];
			bits = fontdes[i].bits;
			cfont = i;
			return (0);
		}
		/* this is a new font */
	if (fnum < 0 || fnum >= MAXF || fontname[fnum].name == 0) {
		fprintf(stderr,
			"Internal error: illegal font %d name %s size\n",
		fontname[fnum].name,fnum,fsize);
		return(-1);
	}
	/* Need to verify the existance of that font/size here*/
	nfontnum = fnum;
	npsize = fsize;
	fontwanted++;
	return (0);
}


getfont()
{
	register int fnum, fsize, fontd;
	int d;
	char cbuf[BUFSIZ];

	if (!fontwanted)
		return(0);
	fnum = nfontnum;
	fsize = npsize;
	sprintf(cbuf, "/usr/lib/vfont/%s.%d",fontname[fnum].name, fsize);
	fontd = open(cbuf, OPENREAD);
	if (fontd == -1) {
		perror(cbuf);
		error(0,"fnum = %d size = %d name = %s\n",
			fnum,fsize,fontname[fnum]);
		fontwanted = 0;
		return (-1);
	}
	if (read(fontd, &header, sizeof header)!=sizeof header || header.magic!=0436)
		fprintf(stderr, "%s: Bad font file", cbuf);
	else {
		cfont = relfont();
		if (((bits=nalloc(header.size+DSIZ+1,1))== NULL)
			&& ((bits=allpanic(header.size+DSIZ+1))== NULL)) {
				fprintf(stderr, "%s: ran out of memory\n", cbuf);
				exit(ABORT);
		} else {
			/*
			 * have allocated one chunk of mem for font, dispatch.
			 * get the dispatch addr, align to word boundary.
			 */
			d = (int) bits+header.size;
			d += 1;
			d &= ~1;
			if (read(fontd, d, DSIZ)!=DSIZ
			  || read(fontd, bits, header.size)!=header.size)
				fprintf(stderr, "bad font header");
			else {
				close(fontd);
				cfnum = fontdes[cfont].fnum = fnum;
				cpsize = fontdes[cfont].psize = fsize;
				fontdes[cfont].bits = bits;
				fontdes[cfont].disp = (struct dispatch *) d;
				dispatch = &fontdes[cfont].disp[0];
				fontwanted = 0;
				return (0);
			}
		}
	}
	close(fontd);
	fontwanted = 0;
	return(-1);
}

int lastloaded	= -1;

relfont()
{
	register int newfont;

	newfont = lastloaded;
	/*
	 * optimization for special font.  since we think that usually
	 * there is only one character at a time from any special math
	 * font, make it the candidate for removal.
	 */
	if (fontdes[newfont].fnum != SPECIALFONT || fontdes[newfont].bits==0)
		if (++newfont>=NFONTS)
			newfont = 0;
	lastloaded = newfont;
	if(fontdes[newfont].bits!=(char *)-1&&fontdes[newfont].bits!=(char *)0) {
		/* fprintf(stderr, "freeing position %d\n", newfont); */
		nfree(fontdes[newfont].bits);
		fontdes[newfont].bits = (char *)0;
	} else
		/* fprintf(stderr, "taking without freeing position %d\n", newfont); */
		;
	fontdes[newfont].bits = 0;
	return (newfont);
}

char *
allpanic(nbytes)
	int nbytes;
{
	register int i;

	for (i = 0; i <= NFONTS; i++)
		if (fontdes[i].bits != (char *)-1 && fontdes[i].bits != (char *)0)
			nfree(fontdes[i].bits);
	lastloaded = cfont;
	for (i = 0; i <= NFONTS; i++) {
		fontdes[i].fnum = fontdes[i].psize = -1;
		fontdes[i].bits = 0;
		cfnum = cpsize = -1;
	}
	return(nalloc(nbytes,1));
}

int	M[] = { 0xffffffff, 0xfefefefe, 0xfcfcfcfc, 0xf8f8f8f8,
		0xf0f0f0f0, 0xe0e0e0e0, 0xc0c0c0c0, 0x80808080, 0x0 };
int	N[] = { 0x00000000, 0x01010101, 0x03030303, 0x07070707,
		0x0f0f0f0f, 0x1f1f1f1f, 0x3f3f3f3f, 0x7f7f7f7f, 0xffffffff };
int	strim[] = { 0xffffffff, 0xffffff00, 0xffff0000, 0xff000000, 0 };

outc(code)
	int code;
{
	char c;				/* character to print */
	register struct dispatch *d;	/* ptr to character font record */
	register char *addr;		/* addr of font data */
	int llen;			/* length of each font line */
	int nlines;			/* number of font lines */
	register char *scanp;		/* ptr to output buffer */
	int scanp_inc;			/* increment to start of next buffer */
	int offset;			/* bit offset to start of font data */
	int i;				/* loop counter */
	register int count;		/* font data ptr */
	register unsigned fontdata;	/* font data temporary */
	register int off8;		/* offset + 8 */

	if (fontwanted)
		getfont();
	c = code;
	d = dispatch+c;
	if (d->nbytes) {
		addr = bits+d->addr;
		llen = (d->left+d->right+7)/8;
		nlines = d->up+d->down;
		i = vpos + d->down;
		if( i > maxX ) maxX = i; /* for flush */
		scanp= ((vpos-d->up-1)*BYTES_PER_LINE+(hpos-d->left)/8)+buf0p;
		if (scanp < &buffer[0])
			scanp += sizeof buffer;
		scanp_inc = BYTES_PER_LINE-llen;
		offset = -((hpos-d->left)&07);
		off8 = offset+8;
		for (i = 0; i < nlines; i++) {
			if (scanp >= &buffer[BUFFER_SIZE])
				scanp -= sizeof buffer;
			count = llen;
			if (scanp + count <= &buffer[BUFFER_SIZE])
				do {
					fontdata = *(unsigned *)addr;
					addr += 4;
					if (count < 4)
						fontdata &= ~strim[count];
					*(unsigned *)scanp |= (fontdata << offset) &~ M[off8];
					scanp++;
					*(unsigned *)scanp |= (fontdata << off8) &~ N[off8];
					scanp += 3;
					count -= 4;
				} while (count > 0);
			scanp += scanp_inc+count;
			addr += count;
		}
		return;
	}
	return;
}

slop_lines(nlines)
int nlines;

/* Output "nlines" lines from the buffer, and clear that section of the  */
/* buffer.	*/

{
	unsigned usize;

	if(!nlines );
/*	rlines = (&buffer[BUFFER_SIZE] - buf0p) / BYTES_PER_LINE; */
	usize = BYTES_PER_LINE * nlines;
	writev(buf0p,usize);
	vclear(buf0p, usize);
	ioctl(OUTFILE, VSETSTATE, pltmode);
}

writev(buf,usize)
char *buf;
unsigned usize;
{
	register tsize = 0;
	while(usize){
		buf += tsize;
		if(usize > (unsigned)(1024 * 1024)){
			tsize = 1024 * 1024;
		}else tsize = usize;
		usize -= tsize;
		if(dbg)fprintf(stderr,"buf = %d size = %d\n",buf,tsize);
		
		if (tsize != write(OUTFILE, buf, tsize)) {
			perror("dver: write failed");
			exit(RESTART);
		}
	}
}

vclear(ptr,nbytes)
char	*ptr;
unsigned	nbytes;
{
	register	tsize = 0;
	while(nbytes){
		if( (unsigned)(16*1024) < nbytes){
			tsize = 16 * 1024;
		}else tsize = nbytes;
		nbytes -= tsize;
		if(dbg)fprintf(stderr,"clearing ptr = %d size = %d\n",ptr,tsize);
		clear(ptr,tsize);
		ptr += tsize;
	}
}

/*ARGSUSED*/
clear(lp, nbytes)
	int *lp;
	int nbytes;
{

	asm("movc5 $0,(sp),$0,8(ap),*4(ap)");

}

char *
nalloc(i, j)
	int i, j;
{
	register char *cp;

	cp = calloc(i, j);
	if(dbg) fprintf(stderr, "allocated %d bytes at %x\n", i * j, cp);
	return(cp);
}

nfree(cp)
	char *cp;
{

	if(dbg) fprintf(stderr, "freeing at %x\n", cp);/* DEBUG */
	free(cp);
}
