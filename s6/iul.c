#
#define BACKSPACE 0
#define UPPERCASE 0
#define	QUOTE	0200
/*
 * iul - a cat like program to indicate underlining for graphic terminals
 *
 * Bill Joy UCB June 22, 1977
 *
 * This program is actually a piece of the editor ex.
 */

#define	LBSIZE	512

char	linebuf[LBSIZE], genbuf[LBSIZE];
int	ibuf[259];
extern	int fout;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	register char *lp;

	fout = dup(1);
	argc--;
	argv++;
	do {
		if (argc > 0) {
			close(ibuf[0]);
			if (fopen(argv[0], ibuf) < 0) {
				perror(argv[0]);
				flush();
				exit(1);
			}
			argc--;
			argv++;
		}
		lp = linebuf;
		for (;;) {
			c = getc(ibuf);
			if (c < 0)
				break;
			if (c == '\n') {
				*lp = 0;
				doulg();
				dographic();
				if (genbuf[0])
					printf("\n%s", genbuf);
				putchar('\n');
				lp = linebuf;
				continue;
			}
			if (lp >= &linebuf[LBSIZE - 2]) {
				flush();
				printf("Input line exceeds 500 characters\n");
				flush();
				exit(1);
			}
			*lp++ = c;
		}
		flush();
	} while (argc > 0);
	flush();
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
/*
				else if (UPPERCASE && ucletter(c)) {
					putchar('\\');
					c = letter(c);
				}
*/
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
			flush();
			printf("Line too long for this filter\n");
			flush();
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

strcpy(oto, from)
	char *oto;
	register char *from;
{
	register char *to;

	to = oto;
	while (*to++ = *from++)
		continue;
	return (oto);
}
