/*
 * expand - expand tabs to equivalent spaces
 */

int	ibuf[259];

extern	int fout;

main(argc, argv)
	int argc;
	char *argv[];
{
	register int c, column;

	fout = dup(1);
	argc--, argv++;
	do {
		if (argc > 0) {
			if (fopen(argv[0], ibuf)) {
				flush();
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		column = 0;
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			switch (c) {
				case '\t':
					do {
						putchar(' ');
						column++;
					} while (column & 07);
					continue;
				case '\b':
					if (column)
						column--;
					putchar('\b');
					continue;
				default:
					putchar(c);
					column++;
					continue;
				case '\n':
					putchar(c);
					column = 0;
					continue;
			}
		}
	} while (argc > 0);
	flush();
}
