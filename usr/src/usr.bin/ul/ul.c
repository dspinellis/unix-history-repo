/*	@(#)vcrt.c	3.13	*/
#include <stdio.h>

#define	IESC	'\033'
#define	SO	'\016'
#define	SI	'\017'
#define	HFWD	'9'
#define	HREV	'8'
#define	FREV	'7'
#define	MAXBUF	512

#define	NORMAL	000
#define	ALTSET	001	/* Reverse */
#define	SUPERSC	002	/* Dim */
#define	SUBSC	004	/* Dim | Ul */
#define	UNDERL	010	/* Ul */
#define	BOLD	020	/* Bold */

int	must_use_uc, must_overstrike;
char	*CURS_UP, *CURS_RIGHT, *CURS_LEFT,
	*ENTER_STANDOUT, *EXIT_STANDOUT, *ENTER_UNDERLINE, *EXIT_UNDERLINE,
	*ENTER_DIM, *ENTER_BOLD, *ENTER_REVERSE, *UNDER_CHAR, *EXIT_ATTRIBUTES;

struct	CHAR	{
	char	c_mode;
	char	c_char;
} ;

char	buf[BUFSIZ];

struct	CHAR	obuf[MAXBUF];
int	col, maxcol;
int	mode;
int	halfpos;
int	upln;
int	iflag;

main(argc, argv)
	int argc;
	char **argv;
{
	int c;
	char *cp, *termtype;
	FILE *f;
	char termcap[1024];
	char *getenv();
	extern int optind;
	extern char *optarg;

	termtype = getenv("TERM");
	if (termtype == NULL || (argv[0][0] == 'c' && !isatty(1)))
		termtype = "lpr";
	while ((c=getopt(argc, argv, "it:T:")) != EOF)
		switch(c) {

		case 't':
		case 'T': /* for nroff compatibility */
				termtype = optarg;
			break;
		case 'i':
			iflag = 1;
			break;

		default:
			fprintf(stderr,
				"Usage: %s [ -i ] [ -tTerm ] file...\n",
				argv[0]);
			exit(1);
		}

	switch(tgetent(termcap, termtype)) {

	case 1:
		break;

	default:
		fprintf(stderr,"trouble reading termcap");
		/* fall through to ... */

	case 0:
		/* No such terminal type - assume dumb */
		strcpy(termcap, "dumb:os:col#80:cr=^M:sf=^J:am:");
		break;
	}
	initcap();
	if (    (tgetflag("os") && ENTER_BOLD==NULL ) ||
		(tgetflag("ul") && ENTER_UNDERLINE==NULL && UNDER_CHAR==NULL))
			must_overstrike = 1;
	setbuf(stdout, buf);
	initbuf();
	if (optind == argc)
		filter(stdin);
	else for (; optind<argc; optind++) {
		f = fopen(argv[optind],"r");
		if (f == NULL) {
			perror(argv[optind]);
			exit(1);
		} else
			filter(f);
	}
	exit(0);
}

filter(f)
FILE *f;
{
	register c;

	while((c = getc(f)) != EOF) switch(c) {

	case '\b':
		if (col > 0)
			col--;
		continue;

	case '\t':
		col = (col+8) & ~07;
		if (col > maxcol)
			maxcol = col;
		continue;

	case '\r':
		col = 0;
		continue;

	case SO:
		mode |= ALTSET;
		continue;

	case SI:
		mode &= ~ALTSET;
		continue;

	case IESC:
		switch (c = getc(f)) {

		case HREV:
			if (halfpos == 0) {
				mode |= SUPERSC;
				halfpos--;
			} else if (halfpos > 0) {
				mode &= ~SUBSC;
				halfpos--;
			} else {
				halfpos = 0;
				reverse();
			}
			continue;

		case HFWD:
			if (halfpos == 0) {
				mode |= SUBSC;
				halfpos++;
			} else if (halfpos < 0) {
				mode &= ~SUPERSC;
				halfpos++;
			} else {
				halfpos = 0;
				fwd();
			}
			continue;

		case FREV:
			reverse();
			continue;

		default:
			fprintf(stderr,
				"Unknown escape sequence in input: %o, %o\n",
				IESC, c);
			exit(1);
		}
		continue;

	case '_':
		if (obuf[col].c_char)
			obuf[col].c_mode |= UNDERL | mode;
		else
			obuf[col].c_char = '_';
	case ' ':
		col++;
		if (col > maxcol)
			maxcol = col;
		continue;

	case '\n':
		flushln();

	default:
		if (c < ' ')	/* non printing */
			continue;
		if (obuf[col].c_char == '\0') {
			obuf[col].c_char = c;
			obuf[col].c_mode = mode;
		} else if (obuf[col].c_char == '_') {
			obuf[col].c_char = c;
			obuf[col].c_mode |= UNDERL|mode;
		} else
			obuf[col].c_mode |= BOLD|mode;
		col++;
		if (col > maxcol)
			maxcol = col;
		continue;
	}
	flushln();
}

flushln()
{
	register lastmode;
	register i;
	int hadmodes = 0;

	lastmode = NORMAL;
	for (i=0; i<maxcol; i++) {
		if (obuf[i].c_mode != lastmode) {
			hadmodes++;
			setmode(obuf[i].c_mode);
			lastmode = obuf[i].c_mode;
		}
		if (obuf[i].c_char == '\0') {
			if (upln) {
				puts(CURS_RIGHT);
			} else
				outc(' ');
		} else
			outc(obuf[i].c_char);
	}
	if (lastmode != NORMAL) {
		setmode(0);
	}
	if (must_overstrike && hadmodes)
		overstrike();
	putchar('\n');
	if (iflag && hadmodes)
		iattr();
	fflush(stdout);
	if (upln)
		upln--;
	initbuf();
}

/*
 * For terminals that can overstrike, overstrike underlines and bolds.
 * We don't do anything with halfline ups and downs, or Greek.
 */
overstrike()
{
	register int i;
	char lbuf[256];
	register char *cp = lbuf;
	int hadbold=0;

	/* Set up overstrike buffer */
	for (i=0; i<maxcol; i++)
		switch (obuf[i].c_mode) {
		case NORMAL:
		default:
			*cp++ = ' ';
			break;
		case UNDERL:
			*cp++ = '_';
			break;
		case BOLD:
			*cp++ = obuf[i].c_char;
			hadbold=1;
			break;
		}
	putchar('\r');
	for (*cp=' '; *cp==' '; cp--)
		*cp = 0;
	for (cp=lbuf; *cp; cp++)
		putchar(*cp);
	if (hadbold) {
		putchar('\r');
		for (cp=lbuf; *cp; cp++)
			putchar(*cp=='_' ? ' ' : *cp);
		putchar('\r');
		for (cp=lbuf; *cp; cp++)
			putchar(*cp=='_' ? ' ' : *cp);
	}
}

iattr()
{
	register int i;
	char lbuf[256];
	register char *cp = lbuf;

	for (i=0; i<maxcol; i++)
		switch (obuf[i].c_mode) {
		case NORMAL:	*cp++ = ' '; break;
		case ALTSET:	*cp++ = 'g'; break;
		case SUPERSC:	*cp++ = '^'; break;
		case SUBSC:	*cp++ = 'v'; break;
		case UNDERL:	*cp++ = '_'; break;
		case BOLD:	*cp++ = '!'; break;
		default:	*cp++ = 'X'; break;
		}
	for (*cp=' '; *cp==' '; cp--)
		*cp = 0;
	for (cp=lbuf; *cp; cp++)
		putchar(*cp);
	putchar('\n');
}

initbuf()
{
	register i;

	for (i=0; i<MAXBUF; i++) {
		obuf[i].c_char = '\0';
		obuf[i].c_mode = NORMAL;
	}
	col = 0;
	maxcol = 0;
	mode &= ALTSET;
}

fwd()
{
	register oldcol, oldmax;

	oldcol = col;
	oldmax = maxcol;
	flushln();
	col = oldcol;
	maxcol = oldmax;
}

reverse()
{
	upln++;
	fwd();
	puts(CURS_UP);
	puts(CURS_UP);
	upln++;
}

initcap()
{
	static char tcapbuf[512];
	char *termtype;
	char *bp = tcapbuf;
	char *getenv(), *tgetstr();

	/* This nonsense attempts to work with both old and new termcap */
	CURS_UP =		tgetstr("up", &bp);
	CURS_RIGHT =		tgetstr("ri", &bp);
	if (CURS_RIGHT == NULL)
		CURS_RIGHT =	tgetstr("nd", &bp);
	CURS_LEFT =		tgetstr("le", &bp);
	if (CURS_LEFT == NULL)
		CURS_LEFT =	tgetstr("bc", &bp);
	if (CURS_LEFT == NULL && tgetflag("bs"))
		CURS_LEFT =	"\b";

	ENTER_STANDOUT =	tgetstr("so", &bp);
	EXIT_STANDOUT =		tgetstr("se", &bp);
	ENTER_UNDERLINE =	tgetstr("us", &bp);
	EXIT_UNDERLINE =	tgetstr("ue", &bp);
	ENTER_DIM =		tgetstr("mh", &bp);
	ENTER_BOLD =		tgetstr("md", &bp);
	ENTER_REVERSE =		tgetstr("mr", &bp);
	EXIT_ATTRIBUTES =	tgetstr("me", &bp);

	if (!ENTER_BOLD && ENTER_REVERSE)
		ENTER_BOLD = ENTER_REVERSE;
	if (!ENTER_BOLD && ENTER_STANDOUT)
		ENTER_BOLD = ENTER_STANDOUT;
	if (!ENTER_UNDERLINE && ENTER_STANDOUT) {
		ENTER_UNDERLINE = ENTER_STANDOUT;
		EXIT_UNDERLINE = EXIT_STANDOUT;
	}
	if (!ENTER_DIM && ENTER_STANDOUT)
		ENTER_DIM = ENTER_STANDOUT;
	if (!ENTER_REVERSE && ENTER_STANDOUT)
		ENTER_REVERSE = ENTER_STANDOUT;
	if (!EXIT_ATTRIBUTES && EXIT_STANDOUT)
		EXIT_ATTRIBUTES = EXIT_STANDOUT;
	
	/*
	 * Note that we use REVERSE for the alternate character set,
	 * not the as/ae capabilities.  This is because we are modelling
	 * the model 37 teletype (since that's what nroff outputs) and
	 * the typical as/ae is more of a graphics set, not the greek
	 * letters the 37 has.
	 */

#ifdef notdef
printf("so %s se %s us %s ue %s me %s\n",
	ENTER_STANDOUT, EXIT_STANDOUT, ENTER_UNDERLINE,
	EXIT_UNDERLINE, EXIT_ATTRIBUTES);
#endif
	UNDER_CHAR =		tgetstr("uc", &bp);
	must_use_uc = (UNDER_CHAR && !ENTER_UNDERLINE);
}

outchar(c)
char c;
{
	putchar(c&0177);
}

puts(str)
char *str;
{
	if (str)
		tputs(str, 1, outchar);
}

static curmode = 0;
outc(c)
char c;
{
	putchar(c);
	if (must_use_uc &&  (curmode&UNDERL)) {
		puts(CURS_LEFT);
		puts(UNDER_CHAR);
	}
}

setmode(newmode)
int newmode;
{
	if (!iflag)
		switch (newmode) {
		case NORMAL:
			switch(curmode) {
			case NORMAL:
				break;
			case UNDERL:
				puts(EXIT_UNDERLINE);
				break;
			default:
				/* This includes standout */
				puts(EXIT_ATTRIBUTES);
				break;
			}
			break;
		case ALTSET:
			puts(ENTER_REVERSE);
			break;
		case SUPERSC:
			/*
			 * This only works on a few terminals.
			 * It should be fixed.
			 */
			puts(ENTER_UNDERLINE);
			puts(ENTER_DIM);
			break;
		case SUBSC:
			puts(ENTER_DIM);
			break;
		case UNDERL:
			puts(ENTER_UNDERLINE);
			break;
		case BOLD:
			puts(ENTER_BOLD);
			break;
		default:
			/*
			 * We should have some provision here for multiple modes
			 * on at once.  This will have to come later.
			 */
			puts(ENTER_STANDOUT);
			break;
		}
	curmode = newmode;
}
/*	@(#)getopt.c	3.2	*/
#define	ERR(s, c)	if(opterr){\
	fputs(argv[0], stderr);\
	fputs(s, stderr);\
	fputc(c, stderr);\
	fputc('\n', stderr);}

int	opterr = 1;
int	optind = 1;
char	*optarg;
char	*index();

int
getopt (argc, argv, opts)
	char **argv, *opts;
{
	static int sp = 1;
	char c;
	char *cp;

	if (sp == 1)
		if (optind >= argc ||
		   argv[optind][0] != '-' || argv[optind][1] == '\0')
			return EOF;
		else if (strcmp(argv[optind], "--") == NULL) {
			optind++;
			return EOF;
		}
		else if (strcmp(argv[optind], "-?") == NULL) {
			optind++;
			return '?';
		}
	c = argv[optind][sp];
	if (c == ':' || (cp=index(opts, c)) == NULL) {
		ERR (": illegal option -- ", c);
		if (argv[optind][++sp] == '\0') {
			optind++;
			sp = 1;
		}
		return '?';
	}
	if (*++cp == ':') {
		if (argv[optind][2] != '\0')
			optarg = &argv[optind++][sp+1];
		else if (++optind >= argc) {
			ERR (": option requires an argument -- ", c);
			sp = 1;
			return '?';
		} else
			optarg = argv[optind++];
		sp = 1;
	}
	else if (argv[optind][++sp] == '\0') {
		sp = 1;
		optind++;
	}
	return c;
}
