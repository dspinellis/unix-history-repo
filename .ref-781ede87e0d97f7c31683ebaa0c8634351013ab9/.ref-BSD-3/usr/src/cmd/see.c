#include <stdio.h>

/*
 * see - a cat like program which prints like the ex "print" command.
 * It always prints backspaces as "^H".
 *
 * Bill Joy UCB July 13, 1978
 *
 */
int	noeol;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	char obuf[BUFSIZ];

	setbuf(stdout, obuf);
	argc--, argv++;
	if (argc > 0 && argv[0][0] == '-')
		noeol++, argc--, argv++;
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == 0) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		for (;;) {
			c = getchar();
			if (c == -1)
				break;
			c &= 0177;
			if (c == 0177)
				putchar('^'), c = '?';
			if (c == 033)
				c = '$';
			if (c < ' ')
				switch (c) {

				case '\n':
					break;

				default:
					putchar('^');
					c |= 0100;
				}
			if (c == '\n' && !noeol)
				putchar('$');
			putchar(c);
			if (c == '\n')
				fflush(stdout);
		}
	} while (argc > 0);
	exit(0);
}
