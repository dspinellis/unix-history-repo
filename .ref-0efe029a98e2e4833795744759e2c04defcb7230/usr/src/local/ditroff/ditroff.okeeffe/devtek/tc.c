#ifndef lint
static char sccsid[] = "@(#)tc.c	1.2	(CWI)	1.2	85/03/26";
#endif lint
/*
 *	drive 4014 scope
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
#include	<ctype.h>

#include "../dev.h"
#define	NFONT	10

int	output	= 0;	/* do we do output at all? */
int	nolist	= 0;	/* output page list if > 0 */
int	olist[20];	/* pairs of page numbers */

int	erase	= 1;
float	aspect	= 1.5;	/* default aspect ratio */
int	(*sigint)();
int	(*sigquit)();

struct dev dev;
struct font *fontbase[NFONT];
short	psizes[]	={ 11, 16, 22, 36, 0};	/* approx sizes available */
short	*pstab		= psizes;
int	nsizes	= 4;
int	pscode[]	={ ';', ':', '9', '8'};
int	nfonts;
int	smnt;	/* index of first special font */
int	nchtab;
char	*chname;
short	*chtab;
char	*fitab[NFONT];
char	*widthtab[NFONT];	/* widtab would be a better name */
char	*codetab[NFONT];	/* device codes */

#define	FATAL	1
#define	BMASK	0377
int	keepon 	= 0;
int	dbg	= 0;
long	lineno	= 0;
int	res	= 972;		/* input assumed computed according to this resolution */
				/* initial value to avoid 0 divide */
FILE	*tf	= stdout;	/* output file */
char	*fontdir	= "/usr/lib/font";
extern char devname[];

FILE *fp	= stdin;	/* input file pointer */

main(argc, argv)
char *argv[];
{
	char buf[BUFSIZ];
	float atof();
	int done();

	setbuf(stdout, buf);
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'T':
			if (strcmp(&argv[1][2], "cat") == 0) {	/* use the old one */
				if (fork() == 0) {
					execv("/usr/bin/oldtc", argv);
					fprintf(stderr, "tc: can't find oldtc\n");
				}
				wait();
				exit(1);
			}
			break;
		case 'a':
			aspect = atof(&argv[1][2]);
			break;
		case 'e':
			erase = 0;
			break;
		case 'o':
			outlist(&argv[1][2]);
			break;
		case 'd':
			dbg = atoi(&argv[1][2]);
			if (dbg == 0) dbg = 1;
			break;
		case 'c':
			keepon = 1;
			break;
		}
		argc--;
		argv++;
	}

	sigint = signal(SIGINT, done);
	sigquit = signal(SIGQUIT, SIG_IGN);
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
			lineno++;
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
			lineno++;
			t_text(buf);
			break;
		case 'D':	/* draw function */
			fgets(buf, sizeof(buf), fp);
			lineno++;
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
			lineno++;
			break;
		case 'x':	/* device control */
			devcntrl(fp);
			lineno++;
			break;
		default:
			error(!FATAL, "unknown input character %o %c\n", c, c);
			while (getc(fp) != '\n')
				;
		}
	}
}

devcntrl(fp)	/* interpret device control functions */
FILE *fp;
{
	char str[20];
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
		loadfont(n, str);
		break;
	}
	while (getc(fp) != '\n')	/* skip rest of input line */
		;
}

fileinit()	/* read in font and code files, etc. */
{
}

fontprint(i)	/* debugging print of font i (0,...) */
{
}

loadcode(n, nw)	/* load codetab on position n (0...); #chars is nw */
int n, nw;
{
}

loadfont(n, s)	/* load font info for font s on position n (1...) */
int n;
char *s;
{
}

#define	ESC	033
#define	MAXY	(3071-100)
#define	US	037	/* text mode */
#define	GS	035	/* graphics mode */
#define	FF	014

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "%c%c%c", US, ESC, ';');	/* reset terminal sensibly */
	fprintf(stderr, "tc: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, " near line %ld\n", lineno);
	if (f)
		done(2);
}


/*
	Here beginneth all the stuff that really depends
	on the 202 (we hope).
*/


char	devname[20]	= "4014";

#define	oput(c)	if (output) putchar(c); else;

int	stopped = 0;
int	ohx	= -1;
int	ohy	= -1;
int	oxb	= -1;
int	oly	= -1;
int	olx	= -1;
int	skip;
int	size	= 1;
int	font	= 1;		/* current font */
int	hpos;		/* horizontal position where we are supposed to be next (left = 0) */
int	vpos;		/* current vertical position (down positive) */

int	horig;		/* h origin of current block; hpos rel to this */
int	vorig;		/* v origin of current block; vpos rel to this */

int	DX	= 10;	/* step size in x for drawing */
int	DY	= 10;	/* step size in y for drawing */
int	drawdot	= '.';	/* draw with this character */
int	drawsize = 1;	/* shrink by this factor when drawing */

t_init(reinit)	/* initialize device */
int reinit;
{
	fflush(stdout);
	stopped = 0;
	if (erase) {
		oput(ESC);
		oput(FF);
		oput(US);
	}
	hpos = vpos = 0;
	setsize(t_size(10));	/* start somewhere */
	sendpt();
}

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
	if (--statep < state)
		error(FATAL, "extra }");
	size = statep->ssize;
	font = statep->sfont;
	hpos = statep->shpos;
	vpos = statep->svpos;
	horig = statep->shorig;
	vorig = statep->svorig;
}

int	np;	/* number of pages seen */
int	npmax;	/* high-water mark of np */
int	pgnum[100];	/* their actual numbers */
long	pgadr[100];	/* their seek addresses */

t_page(n)	/* do whatever new page functions */
{
	long ftell();
	int c, m, i;
	char buf[100], *bp;

	pgnum[np++] = n;
	pgadr[np] = ftell(fp);
	if (np > npmax)
		npmax = np;
	if (output == 0) {
		output = in_olist(n);
		t_init(1);
		return;
	}
	/* have just printed something, and seen p<n> for next one */
	vgoto(11 * res - 100);
	sendpt();
	oput(US);
	fflush(stdout);


	if (keepon) {
		t_init(1);
		return;
	}
  next:
	for (bp = buf; (*bp = readch()); )
		if (*bp++ == '\n')
			break;
	*bp = 0;
	switch (buf[0]) {
	case 0:
	case 'q':
		done();
		break;
	case '\n':
		if (stopped)
			done();
		output = in_olist(n);
		t_init(1);
		return;
	case '!':
		callunix(&buf[1]);
		fputs("!\n", stderr);
		break;
	case 'e':
		erase = 1 - erase;
		break;
	case 'a':
		aspect = atof(&buf[1]);
		break;
	case '-':
	case 'p':
		m = atoi(&buf[1]) + 1;
		if (fp == stdin) {
			fputs("you can't; it's not a file\n", stderr);
			break;
		}
		if (np - m <= 0) {
			fputs("too far back\n", stderr);
			break;
		}
		np -= m;
		fseek(fp, pgadr[np], 0);
		output = 1;
		t_init(1);
		return;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		m = atoi(&buf[0]);
		for (i = 0; i < npmax; i++)
			if (m == pgnum[i])
				break;
		if (i >= npmax || fp == stdin) {
			fputs("you can't\n", stderr);
			break;
		}
		np = i + 1;
		fseek(fp, pgadr[np], 0);
		output = 1;
		t_init(1);
		return;
	case 'o':
		outlist(&buf[1]);
		output = 0;
		t_init(1);
		return;
	case '?':
		fputs("!cmd	unix cmd\n", stderr);
		fputs("p	print this page again\n", stderr);
		fputs("-n	go back n pages\n", stderr);
		fputs("n	print page n (previously printed)\n", stderr);
		fputs("o...	set the -o output list to ...\n", stderr);
		fputs("q	quit\n", stderr);
		fputs("en	n=0 -> don't erase; n=1 -> erase\n", stderr);
		fputs("an	sets aspect ratio to n\n", stderr);
		break;
	default:
		fputs("?\n", stderr);
		break;
	}
	goto next;
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

t_font(s)	/* convert string to internal font number */
char *s;
{
	int n;

	n = atoi(s);
	if (n < 1 || n > nfonts)
		n = 1;
	return(n);
}

t_text(s)	/* print string s as text */
char *s;
{
	int c, w;
	char str[100];

	if (!output)
		return;
	w = res / 2 * pstab[size-1] / 72;
	while ((c = *s++) != '\n') {
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
		hmot(w);
	}
}

t_reset(c)
{
	int n;

	output = 1;
	fflush(stdout);
	if (c == 's') {
		stopped = 1;
		t_page(9999);
	}
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
	hgoto(hpos + n);
}

hflush()	/* actual horizontal output occurs here */
{
	if (output)
		sendpt();
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
char *s;
{
	int i;
	char *p;
	extern char *spectab[];
	static char prev[10] = "";
	static int previ;

	if (!output)
		return;
	if (strcmp(s, prev) != 0) {
		previ = -1;
		for (i = 0; spectab[i] != 0; i += 2)
			if (strcmp(spectab[i], s) == 0) {
				strcpy(prev, s);
				previ = i;
				break;
			}
	}
	if (previ >= 0) {
		hflush();
		oput(US);
		for (p = spectab[previ+1]; *p; p++)
			oput(*p);
	} else
		prev[0] = 0;
}

put1(c)	/* output char c */
int c;
{
	if (!output)
		return;
	hflush();
	oput(US);
	oput(c);
}

setsize(n)	/* set point size to n (internal) */
int n;
{

	if (!output)
		return;
	if (n == size)
		return;	/* already there */
	oput(ESC);
	oput(pscode[n-1]);
	size = n;
}

t_fp(n, s)	/* font position n now contains font s */
int n;
char *s;
{
}

setfont(n)	/* set font to n */
int n;
{
}

done()
{
	output = 1;
	hgoto(0);
	vgoto(11 * res - 100);	/* bottom of page */
	sendpt();
	oput(US);
	oput(ESC);
	oput(';');
	oput(US);
	fflush(stdout);
	exit(0);
}

callunix(line)
char line[];
{
	int rc, status, unixpid;
	if( (unixpid=fork())==0 ) {
		signal(SIGINT,sigint); signal(SIGQUIT,sigquit);
		close(0); dup(2);
		execl("/bin/sh", "-sh", "-c", line, 0);
		exit(255);
	}
	else if(unixpid == -1)
		return;
	else{	signal(SIGINT, SIG_IGN); signal(SIGQUIT, SIG_IGN);
		while( (rc = wait(&status)) != unixpid && rc != -1 ) ;
		signal(SIGINT, done); signal(SIGQUIT,sigquit);
	}
}
readch(){
	char c;
	if (read(2,&c,1)<1) c=0;
	return(c);
}
sendpt(){
	int hy,xb,ly,hx,lx;
	int xx, yy;
	float fx, fy;


	fx = hpos + horig;
	fy = vpos + vorig;
	xx = (fx * MAXY / 11) / res * aspect + 0.5;
	yy = MAXY - (fy * MAXY / 11) / res + 0.5;
	oput(GS);
	hy = ((yy>>7) & 037);
	xb = ((xx & 03) + ((yy<<2) & 014) & 017);
	ly = ((yy>>2) & 037);
	hx = ((xx>>7) & 037);
	lx = ((xx>>2) & 037);
	if(hy != ohy)oput(hy | 040);
	if(xb != oxb)oput(xb | 0140);
	if((ly != oly) || (hx != ohx) || (xb != oxb))
		oput(ly | 0140);
	if(hx != ohx)oput(hx | 040);
	oput(lx | 0100);
	ohy = hy;
	oxb = xb;
	oly = ly;
	ohx = hx;
	olx = lx;
}

char *spectab[] ={
	"em", "--",
	"en", "-",
	"hy", "-",
	"ff", "ff",
	"fi", "fi",
	"fl", "fl",
	"Fi", "ffi",
	"Fl", "ffl",
	"ct", "\033\016Z\bM\033\017",	/*cent sign*/
	"de", "\033\016J\033\017",	/*degree*/
	"dg", "\033\016M\b_\033\017",	/*dagger*/
	"rg", "\033\016O\b&\033\017",	/*registered*/
	"bu", "\033\016O\b~\033\017",	/*bullet*/
	"fm", "'",
	"co", "\033\016O\b#\033\017",	/*copyright*/
	"sq", "\033\016L\033\017",	/*square*/
	"*q", "\033\016(\bM\033\017",	/*psi*/
	"*h", "\033\016o\b_\033\017",	/*theta*/
	"*n", "v\b)",	/*nu*/
	"*m", "\033\016V\b,\033\017",	/*mu*/
	"*l", "\033\016)\b?\033\017",	/*lambda*/
	"*i", "\033\016I\033\017",	/*iota*/
	"*z", "S\b\033\016Z\033\017",	/*zeta*/
	"*s", "o\b\'",	/*sigma*/
	"*d", "o\b\033\0165\033\017",	/*delta*/
	"*b", "\033\016b\033\017",	/*beta*/
	"*c", "\033\016e\bc\033\017",	/*xi*/
	"*y", "j\b\033\016C\033\017",	/*eta*/
	"*f", "\033\016O\bM\033\017",	/*phi*/
	"*u", "\033\016(\033\017",	/*upsilon*/
	"*k", "\033\016k\033\017",	/*kappa*/
	"*p", "T\b\033\016S\033\017",	/*pi*/
	"da", "\033\016U\033\017",	/*down arrow*/
	"*a", "\033\016A\033\017",	/*alpha*/
	"or", "|",
	"*x", "l\b/",	/*chi*/
	"*e", "\033\016E\033\017",	/*epsilon*/
	"*o", "\033\016O\033\017",	/*omicron*/
	"<-", "\033\016[\033\017",	/*left arrow*/
	"*r", "\033\016R\033\017",	/*rho*/
	"ua", "\033\016Y\033\017",	/*up arrow*/
	"*t", "\033\016N\033\017",	/*tau*/
	"ul", "_",
	"ru", "_",
	"\\_", "_",
	"*Q", "I\b\033\016(\033\017",	/*Psi*/
	"bs", "\033\016O\bJ\033\017",	/*bell system sign*/
	"if", "\033\016W\bX\033\017",	/*infinity*/
	"*g", "`\b/",	/*gamma*/
	"ip", "\033\016X\bF\033\017",	/*improper superset*/
	"pt", "\033\016A\033\017",	/*proportional to*/
	"rh", "\033\016\\\b]\033\017",	/*right hand*/
	"*w", "\033\016W\033\017",	/*omega*/
	"gr", "\033\016G\033\017",	/*gradient*/
	"*F", "I\033\016\bO\033\017",	/*Phi*/
	"*H", "O\b=",	/*Theta*/
	"*W", "O\b_",	/*Omega*/
	"cu", "\033\016V\033\017",	/*cup (union)*/
	"rn", "\033\016@\033\017",	/*root en*/
	"ts", "s",	/*terminal sigma*/
	"*L", "\033\016)\bK\033\017",	/*Lambda*/
	"\\-", "-",
	"*G", "\033\016S\bK\033\017",	/*Gamma*/
	"is", "\033\016i\033\017",	/*integral sign*/
	"Sl", "l",
	"*P", "\033\016t\b'\033\017",	/*Pi*/
	"sb", "\033\016Z\033\017",	/*subset of*/
	"sp", "\033\016X\033\017",	/*superset of*/
	"ap", "\033\016T\033\017",	/*approximates*/
	"pd", "o\b`",	/*partial derivative*/
	"*D", "\033\016H\033\017",	/*Delta*/
	"sr", "\033\016I\b'\033\017",	/*square root*/
	"*S", ">\b\033\016F\b@\033\017",	/*Sigma*/
	"~~", "\033\016T\bF\033\017",	/*approx =*/
	"*C", "\033\016_\bF\b@\033\017",	/*Xi*/
	"sl", "/",
	"ca", "\033\016C\033\017",	/*cap (intersection)*/
	"U", "\033\016y\033\017",	/*Upsilon*/
	"no", "\033\016|\033\017",	/*not*/
	"rc", "|",	/*right ceiling (rt of ")*/
	"lt", "|",	/*left top (of big curly)*/
	"bv", "|",	/*bold vertical*/
	"lk", "|",	/*left center of big curly bracket*/
	"lb", "|",	/*left bottom*/
	"rt", "|",	/*right top*/
	"rk", "|",	/*right center of big curly bracket*/
	"rb", "|",	/*right bot*/
	"rf", "|",	/*right floor (rb of ")*/
	"lf", "|",	/*left floor (left bot of big sq bract)*/
	"lc", "|",	/*left ceiling (lt of ")*/
	"mu", "\033\016=\033\017",	/*multiply*/
	"di", "\033\016+\033\017",	/*divide*/
	"+-", "+\b_",	/*plus-minus*/
	"<=", "\033\016$\033\017",	/*<=*/
	">=", "\033\016^\033\017",	/*>=*/
	"==", "=\b_",	/*identically equal*/
	"!=", "\033\016*\033\017",	/*not equal*/
	"aa", "'",
	"ga", "`",
	"lh", "\033\016|\b[\033\017",	/*left hand*/
	"mo", "\033\016c\b_\033\017",	/*member of*/
	"es", "\033\016O\b/\033\017",	/*empty set*/
	"dd", "\033\016%\bM\033\017",	/*dbl dagger*/
	"br", "|",	/*box rule*/
	"vr", "|",	/* vertical rule */
	"ib", "\033\016Z\bF\033\017",	/*improper subset*/
	"ci", "\033\016O\033\017",	/*circle*/
	"eq", "=",
	"pl", "+",
	"mi", "-",
	"12", "1/2",
	"14", "1/4",
	"34", "3/4",
	"->", "\033\016]\033\017",	/*right arrow*/
	"sc", "g\b\033\016C\033\017",	/*section mark*/
	"**", "*",
	"l.", ".",
	"L.", ".",
	"bx", "[\b]",
	"ob", "o",	/* open bullet */
	"cd", ",",	/* cedilla */
	"..", "\033\016!\033\017",	/* umlaut */
	0, 0,
};
