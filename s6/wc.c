/*
 * wc - line and word count
 *
 * Modified by Bill Joy UCB August 5, 1977
 *
 * Also prints character count thus output is
 *
 *	  lines   words   characters	  file
 *
 * The option '-' suppresses all the noise and gives
 * word counts only.
 */

int	ibuf[259];

long	wordct, linect, charct;
long	twordct, tlinect, tcharct;

char	*name;

char	summary;
char	simple;

main(argc, argv)
	int argc;
	char *argv[];
{
	int i, token;
	register c;

	argc--, argv++;
	if (argc && argv[0][0] == '-')
		simple = 1, argc--, argv++;
	summary = argc > 1;
	do {
		if (argc > 0) {
			close (0);
			if (fopen(argv[0], ibuf) < 0) {
				perror(argv[0]);
				argc--, argv++;
				continue;
			}
			argc--, name = *argv++;
		} else
			name = "";
		linect = 0;
		wordct = 0;
		charct = 0;
		token = 0;
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				break;
			charct++;
			if (c == '\n') {
				linect++;
				token = 0;
				continue;
			}
			if (c == ' ' || c == '\t')
				token = 0;
			if (c > ' ' && c < 0177) {
				if (token == 0)
					wordct++;
				token = 1;
				continue;
			}
		}
		if (simple)
			printf("%7ld\n", wordct);
		else
			printf("%7ld %7ld %7ld %s\n", linect, wordct, charct, name);
		tlinect =+ linect;
		twordct =+ wordct;
		tcharct =+ charct;
	} while (argc > 0);
	if (summary == 0 || simple)
		exit (0);
	printf("%7ld %7ld %7ld total\n", tlinect, twordct, tcharct);
	exit(0);
}
