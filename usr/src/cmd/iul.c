#include <stdio.h>

#define BACKSPACE 0
#define	QUOTE	0200
/*
 * iul - a cat like program to indicate underlining for graphic terminals
 *
 * Bill Joy UCB June 22, 1977
 *
 * This program was a piece of the editor ex.
 */

#define	LBSIZE	512

char	linebuf[LBSIZE], genbuf[LBSIZE];
char	*strcpy();

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	register char *lp;
	char obuf[BUFSIZ];

	setbuf(stdout, obuf);
	argc--;
	argv++;
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
					if (gp >= &genbuf[LBSIZE - 2])
						goto ovflo;
					col++;
				}
				break;
			default:
				if (gp >= maxgp)
					break;
				c =| (*gp & QUOTE);
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
		if (gp >= &genbuf[LBSIZE - 2]) {
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
			c =& 0177;
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
