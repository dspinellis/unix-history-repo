#
/*
 * ulpr - a filter to speed underlined output to lpr
 *
 * Author : Bill Joy UCB June 21, 1977
 *
 * This program replaces lpr for stupid printer drivers who
 * waste enroormous amounts of time underlining.
 * It has one bad habit - overstriking with anything other than '_'
 * disappears.
 */
#define LINSIZ	512

char	linbuf[LINSIZ], ulinbuf[LINSIZ];
char	anyul;

int	ibuf[259];
extern	fout;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *lp, *ulp;
	char *maxlp;
	int pvec[2], i;
	register c;
	int col;

	pipe(pvec);
	i = fork();
	if (i == -1) {
		perror("lpr");
		exit(1);
	}
	if (i == 0) {
		close(pvec[1]);
		close(0);
		dup(pvec[0]);
		execl("/bin/lpr", "lpr", 0);
		execl("/usr/bin/lpr", "lpr", 0);
		perror("/usr/bin/lpr");
		exit(1);
	}
	close(1);
	dup(pvec[1]);
	close(pvec[0]);
	fout = dup(1);
	argc--;
	argv++;
	do {
		if (argc > 0) {
			if (fopen(argv[0], ibuf) < 0) {
				flush();
				perror(argv[0]);
				flush();
				exit(1);
			}
			argc--;
			argv++;
		}
line:
		lp = linbuf;
		ulp = ulinbuf;
		col = 0;
		maxlp = lp;
		for (;;) {
			c = getc(ibuf);
			if (c == -1) {
				flush();
				break;
			}
			switch (c) {
				case ' ':
					*lp++ = c;
					*ulp++ = ' ';
					col++;
					break;
				case '\t':
					do {
						col++;
						*lp++ = ' ';
						*ulp++ = ' ';
					} while (col & 7);
					break;
				case '\n':
					if (lp > maxlp)
						maxlp = lp;
					*maxlp = 0;
					while (ulp >= ulinbuf)
						switch (*--ulp) {
							case ' ':
								--lp;
								continue;
							case '_':
								--lp;
								if (*lp == ' ') {
									*lp = '_';
									continue;
								}
							default:
								ulp[1] = 0;
								if (ulinbuf[0])
									printf("%s\r", ulinbuf);
								ulp = 0;
						}
					printf("%s\n", linbuf);
					goto line;
				case '_':
					if (lp >= maxlp)
						*lp = ' ';
					lp++;
					*ulp++ = '_';
					col++;
					break;
				case '\b':
					if (lp > maxlp)
						maxlp = lp;
					if (lp >= linbuf) {
						lp--;
						ulp--;
						col--;
					}
					break;
				default:
					col++;
					if (lp >= maxlp)
						*ulp = ' ';
					else if (*lp != ' ')
						*ulp = *lp;
					*lp++ = c;
					ulp++;
					break;
			}
		}
	} while (argc > 0);
	flush();
}
