/*	lpq.c	4.2	83/05/13	*/
/*
 * Spool Queue examination program
 *
 * lpq [+[n]] [-Pprinter] [user...] [job...]
 *
 * + means continually scan queue until empty
 * -P used to identify printer as per lpr/lprm
 */

#include "lp.h"

char	*user[MAXUSERS];	/* users to process */
int	users;			/* # of users in user array */
int	requ[MAXREQUESTS];	/* job number of spool entries */
int	requests;		/* # of spool requests */

int	repeat;			/* + flag indicator */
int	slptime = 30;		/* pause between screen refereshes */
int	lflag;			/* long output option */

/*
 * Termcap stuff for fancy display
 */
#ifdef TERMCAP
struct sgttyb sbuf;
unsigned ospeed;
int	dumb;			/* whether to use capabilities */
char	PC;			/* pad character for output */
char	*UP;			/* up one line */
char	*BC;			/* backspace character, other than \b */
char	*CM;			/* cursor motion */
char	*CL;			/* clear display */
char	*TI;			/* terminal init for CM */
char	*TE;			/* terminal clear for CM */
char	*SO;			/* stand out start */
char	*SE;			/* stand out end */

char	*tgetstr();
int	putch();		/* for tputs' */
#endif

main(argc, argv)
	char *argv[];
{
	register char *arg;
	register int n;

	name = argv[0];
	gethostname(host, sizeof(host));

	while (--argc) {
		if ((arg = *++argv)[0] == '+') {
			if (arg[1] != '\0')
				if ((n = atoi(&arg[1])) > 0)
					slptime = n;
			repeat++;
		} else if (arg[0] == '-')
			switch (arg[1]) {
			case 'P':		/* printer name */
				printer = &arg[2];
				break;

			case 'l':		/* long output */
				lflag++;
				break;

			default:
				usage();
		} else {
			if (isdigit(arg[0])) {
				if (requests >= MAXREQUESTS)
					fatal("too many requests");
				requ[requests++] = atoi(arg);
			} else {
				if (users >= MAXUSERS)
					fatal("too many users");
				user[users++] = arg;
			}
		}
	}
	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
#ifdef TERMCAP
	dumb = termcap();
#endif

	if (repeat) {
#ifdef TERMCAP
		if (TI)
			tputs(TI, 0, putch);
#endif
		do {
#ifdef TERMCAP
			if (!dumb) {
				tputs(CL, 0, putch);
				tputs(tgoto(CM, 0, 0), 0, putch);
			}
#endif
			if ((n = displayq(lflag)) > 0)
				sleep(slptime);
		} while (n > 0);
#ifdef TERMCAP
		if (!dumb) {
			standout(stdout, "Hit return to continue");
			while (getchar() != '\n');
			if (TE)
				tputs(TE, 0, putch);
		}
#endif
	} else
		displayq(lflag);
}

usage()
{
	printf("usage: lpq [-Pprinter] [-l] [+[n]] [user...] [job...]\n");
	exit(1);
}

/*
 * If we have the capability, print this in standout mode
 */
standout(f, s, a1, a2)
	FILE *f;
	char *s;
{
#ifdef TERMCAP
	if (SO)
		tputs(SO, 0, putch);
	fprintf(f, s, a1, a2);
	if (SO && SE)
		tputs(SE, 0, putch);
#else
	fprintf(f, s, a1, a2);
#endif
}

#ifdef TERMCAP
char *
capstrings[] = {
	"bc", "cl", "cm", "so", "se", "ti", "te", "up",
	0
};

char **
caps[] = {
	&BC, &CL, &CM, &SO, &SE, &TI, &TE, &UP,
};

/*
 * All we need from termcap is to clear screen and
 *   position cursor at the top; if these aren't available
 *   we say the terminal is dumb and let things scroll
 */
termcap()
{
	char *term, tbuf[BUFSIZ];
	static char buf[BUFSIZ/2];
	register short columns;
	char *bp = buf;
	register char **p, ***q, *cp;
	extern int SIZCOL;

	ioctl(0, TIOCGETP, (char *)&sbuf);
	ospeed = sbuf.sg_ospeed;
	if ((term = getenv("TERM")) != NULL && tgetent(tbuf, term) > 0) {
		for (p = capstrings, q = caps; *p != NULL; p++, q++)
			**q = tgetstr(*p, &bp);
		if ((cp = tgetstr("pc", &bp)) != NULL)
			PC = *cp;
		if ((columns = tgetnum("co")) >= 0)
			SIZCOL = columns-18;
	}
	return(CL == NULL || CM == NULL);
}

/*
 * Putchar writearound for tputs
 */
putch(c)
	char c;
{
	putchar(c);
}
#endif
