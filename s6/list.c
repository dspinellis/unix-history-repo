#
/*
 * list - a cat like program which prints lines like the editor 'list' command
 *
 * Bill Joy UCB June 18, 1977
 *
 * Option - suppresses $ at end of line
 */
int	ibuf[259];
extern	fout;

char	nodols;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;

	fout = dup(1);
	argc--, argv++;
	if (argc > 1 && argv[0][0] == '-')
		nodols++, argc--, argv++;
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
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			if (c < ' ')
				switch (c) {
					case '\n':
						if (!nodols)
							putchar('$');
						break;
					case '\t':
						c = '>';
						break;
					case '\b':
						c = '<';
						break;
					default:
						putchar('\\');
						putchar((c >> 3) | '0');
						c =& 07;
						c =| '0';
						break;
				}
			putchar(c);
		}
	} while (argc > 0);
	flush();
	exit(0);
}
