/*
 * pcc - interpret Pascal carriage control
 *
 * Bill Joy UCB January/July 1977
 */

char	*progname;
int	owenl;
int	ibuf[259];
extern	fout;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;

	fout = dup(1);
	argc--, argv++;
	do {
		if (argc > 0) {
			close(0);
			if (fopen(argv[0], ibuf) < 0) {
				flush();
				if (owenl)
					putchar('\n');
				perror(argv[0]);
				flush();
				exit(1);
			}
			argv++, argc--;
		}
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			if (c == '\014') {
feed:
				putchar(c);
				if (owenl) {
					putchar('\n');
					owenl = 0;
				}
				continue;
			}
			switch (c) {
				case '1':
					if (owenl) {
						putchar('\014');
						putchar('\n');
					}
					break;
				case '0':
					putchar('\n');
				case ' ':
					if (owenl)
						putchar('\n');
					break;
				case '+':
					putchar('\r');
					break;
				default:
					if (owenl)
						putchar('\n');
					break;
				case '\n':
					putchar('\n');
					continue;
			}
			owenl = 1;
			for (;;) {
				c = getc(ibuf);
				if (c == -1 || c == '\n')
					break;
				if (c == '\014')
					goto feed;
				putchar(c);
			}
		}
	} while (argc > 0);
	if (owenl)
		putchar('\n');
	flush();
	exit(0);
}
