static char *sccsid = "@(#)ul.c	4.1 (Berkeley) 10/1/80";
/*
 * ul
 */
#include <stdio.h>

char	buf[BUFSIZ];
char	isul[BUFSIZ];
char	termcap[1024];
char	ulbuf[BUFSIZ];
char	*stul, *endul, *chul;
char	*backspace;
char	*termtype;
int	outc();
char	*tgetstr();
char	*getenv();

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	char *cp;
	FILE *f;

	argc--, argv++;
	termtype = getenv("TERM");
	if (termtype == NULL)
		termtype = "dumb";
	while (argc > 0 && argv[0][0] == '-') {
		switch(argv[0][1]) {

		case 't':
		case 'T': /* for nroff compatibility */
			if (argv[0][2])
				termtype = &argv[0][2];
			else {
				termtype = argv[1];
				argc--;
				argv++;
			}
			break;
		case 'i':
			argc--, argv++;
			iul(argc, argv);
			exit(0);

		default:
			printf("Usage: ul [ -i ] [ -tTerm ] file...\n");
			exit(1);
		}
	}
	switch(tgetent(termcap, termtype)) {

	case 1:
		if (tgetflag("os"))
			execv("/bin/cat",argv);
		cp = ulbuf;
		if ((backspace = tgetstr("bc",&cp)) == NULL)
			backspace = "\b";
		/*
		 * Handle terminals that have start underline/stop
		 * underline sequences, as well as those with
		 * underline char sequences (we assume the sequence
		 * moves the cursor forward one character).
		 * If we can't find underline sequences, we
		 * settle for standout sequences.
		 */
		if ((chul=tgetstr("uc",&cp)) == NULL)
			chul = "";
		if ((stul=tgetstr("us",&cp)) == NULL && !tgetflag("ul") &&
		    (!*chul) && (stul=tgetstr("so",&cp)) == NULL)
			stul = "";
		if ((endul=tgetstr("ue",&cp)) == NULL && !tgetflag("ul") &&
		    (!*chul) && (endul=tgetstr("se",&cp)) == NULL)
			endul = "";
		if (chul==0&&stul==0&&endul==0&&tgetflag("ul"))
			execv("/bin/cat",argv);
		break;

	default:
		fprintf(stderr,"trouble reading termcap");
		/* fall through to ... */

	case 0:
		/* No such terminal type - assume dumb */
		stul = endul = chul = "";
		break;
	}
	if (argc == 0)
		filter(stdin);
	else for (i=0; i<argc; i++) {
		f = fopen(argv[i],"r");
		if (f == NULL) {
			perror(argv[i]);
			exit(1);
		} else
			filter(f);
	}
	exit(0);
}

filter(f)
FILE *f;
{
	register int p, n;
	register char c;
	int state;

	n = 0;
	for (;;) {
		p = 0;
		for (p=0; p<n; p++) {
			buf[p] = '\0';
			isul[p] = 0;
		}
		p = n = 0;

		for (;;) {
			c = getc(f);
			if (c==EOF)
				break;
			if (c=='\b') {
				if (p > 0) {
					p--;
				}
			} else if (c=='_' && isul[p]==0 && buf[p]) {
				isul[p] = 1;
				p++;
			} else {
				if (buf[p] == '_')
					isul[p] = 1;
				buf[p] = c;
				p++;
				if (n < p)
					n = p;
			}
			if (c=='\n')
				break;
		}

		state = 0;
		for (p=0; p<n; p++) {
			if (isul[p] != state)
				tputs(isul[p] ? stul : endul, 1, outc);
			state = isul[p];
			putchar(buf[p]);
			if (isul[p] && *chul) {
				printf("%s",backspace);
				tputs(chul, 1, outc);
			}
		}
		if (c==EOF) break;
	}
}

outc(c)
char c;
{
	putchar(c);
}

#define BACKSPACE 0
#define	QUOTE	0200

char	linebuf[BUFSIZ], genbuf[BUFSIZ];
char	*strcpy();

iul(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	register char *lp;

	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--; argv++;
		}
		while (fgets(linebuf, sizeof linebuf, stdin) != 0) {
			for (lp = linebuf; *lp; lp++)
				continue;
			*--lp = 0;
			doulg();
			dographic();
			if (genbuf[0])
				printf("\n%s", genbuf);
			putchar('\n');
			fflush(stdout);
		}
	} while (argc > 0);
	exit(0);
}

dographic()
{
	register char *lp;
	register c;

	for (lp = linebuf; c = *lp++;) {
		switch (c) {
			case '\b':
				if (BACKSPACE == 0)
					c = '?';
				break;
			default:
				if (c < ' ' || c == 0177)
					c = '?';
				break;
			case '\t':
				break;
		}
		putchar(c);
	}
}

doulg()
{
	register char *lp, *gp;
	char *maxgp;
	register c;
	char csw;
	int col;

	gp = genbuf;
	*gp = 0;
	maxgp = gp;
	col = 0;
	for (lp = linebuf; c = *lp++;) {
		switch (c) {
			case '\t':
				while ((col & 7) != 7) {
					*gp++ = ' ';
					if (gp >= &genbuf[BUFSIZ - 2])
						goto ovflo;
					col++;
				}
				break;
			default:
				if (gp >= maxgp)
					break;
				c |= (*gp & QUOTE);
				break;
			case '_':
				if (gp >= maxgp)
					c = QUOTE;
				else
					c = *gp | QUOTE;
				break;
			case '\b':
				if (gp > genbuf) {
					gp--;
					col--;
				}
				continue;
		}
		if (gp >= &genbuf[BUFSIZ - 2]) {
ovflo:
			fprintf(stderr, "Line too long\n");
			exit(1);
		}
		*gp++ = c;
		if (gp > maxgp)
			maxgp = gp;
		col++;
	}
	*maxgp = 0;
	strcpy(linebuf, genbuf);
	for (lp = linebuf, gp = genbuf; c = *lp; gp++, lp++)
		if (c & QUOTE) {
			c &= 0177;
			if (c == 0)
				*lp = '_', *gp = ' ';
			else
				*lp = c, *gp = '-';
		} else
			*gp = ' ';
	--gp;
	while (gp >= genbuf && *gp == ' ')
		--gp;
	gp[1] = 0;
}
