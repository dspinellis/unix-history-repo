/*
 *
 * ul - General underline filter. Converts underlines by
 * the standard backspacing method to the code used by the
 * particular terminal to underline.
 *
 */
#include <stdio.h>
char buf[BUFSIZ];
char isul[BUFSIZ];
char termcap[1024];
char ulbuf[BUFSIZ];
char *stul, *endul, *chul;
char *backspace;
char *termtype;
int outc();
char *tgetstr();
char *getenv();

main(argc,argv) char **argv; {
	register int i;
	char *cp;
	FILE *f;

	/* Figure out kind of terminal and set up special strings. */
	termtype = getenv("TERM");
	if (termtype == NULL)
		termtype = "dumb";
	while (argc >= 2 && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 't':
		case 'T': /* for nroff compatibility */
			if (argv[1][2])
				termtype = &argv[1][2];
			else {
				termtype = argv[2];
				argc--;
				argv++;
			}
			break;
		default:
			printf("Bad switch: %s\n",argv[1]);
			exit(1);
		}
	}
	switch(tgetent(termcap, termtype)) {
		case 1:	/* All is well */
			/* Terminals that don't need any help. */
			if (tgetflag("ul") || tgetflag("os"))
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
			if (	(chul=tgetstr("uc",&cp)) == NULL)
					chul = "";
			if (	(stul=tgetstr("us",&cp)) == NULL &&
				(!*chul) && (stul=tgetstr("so",&cp)) == NULL)
					stul = "";
			if (	(endul=tgetstr("ue",&cp)) == NULL &&
				(!*chul) && (endul=tgetstr("se",&cp)) == NULL)
					endul = "";
			break;
		default:/* error opening/reading termcap */
			fprintf(stderr,"trouble reading termcap");
			/* fall through to ... */
		case 0:	/* No such terminal type - assume dumb */
			stul = endul = chul = "";
			break;
	}
	if (argc < 2) filter(stdin);
	else for (i=1; i<argc; i++) {
		f = fopen(argv[i],"r");
		if (f == NULL) {
			printf("Can't open %s\n",argv[i]);
			exit(1);
		} else filter(f);
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
			if (c==EOF) break;
			if (c=='\b') {
				if (p > 0) {
					p--;
				}
			} else if (c=='_' && isul[p]==0 && buf[p]) {
				isul[p] = 1;
				p++;
			} else {
				if (buf[p] == '_') {
					isul[p] = 1;
				}
				buf[p] = c;
				p++;
				if (n < p) n = p;
			}
			if (c=='\n') break;
		}

		state = 0;
		for (p=0; p<n; p++) {
			if (isul[p] != state)
				tputs(isul[p] ? stul : endul, 1, outc);
			state = isul[p];
			outc(buf[p]);
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
