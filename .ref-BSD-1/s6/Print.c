#
/*
 * p(rint) - cat to a crt with non-graphics escaped as '?'s like 'ex' and 'pc'
 *
 * Bill Joy UCB June 29, 1977
 */
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
			if (fopen(argv[0], ibuf) < 0) {
				flush();
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			if (c < ' ' && c != '\n' && c != '\t')
				c = '?';
			putchar(c);
		}
	} while (argc > 0);
	flush();
	exit(0);
}
