#
/*
 * number - a cat like program which prints lines like the editor '#'' command
 *
 * Bill Joy UCB June 28, 1977
 */
int	ibuf[259];
extern	fout;

int	lino;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	register lastc;

	fout = dup(1);
	argc--, argv++;
	do {
		if (argc > 0) {
			if (fopen(argv[0], ibuf) < 0) {
				flush();
				perror(argv[0]);
				flush();
				exit(1);
			}
			argc--, argv++;
		}
		lastc = '\n';
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			if (lastc == '\n')
				printf("%6d  ", ++lino);
			lastc = c;
			putchar(c);
		}
	} while (argc > 0);
	flush();
	exit(0);
}
