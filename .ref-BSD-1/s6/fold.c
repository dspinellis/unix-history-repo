/*
 * fold - fold long lines for finite output devices
 *
 * Bill Joy UCB June 28, 1977
 */

int	fold 80;
extern	fout;
int	ibuf[259];

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;

	argc--, argv++;
	fout = dup(1);
	if (argc > 0 && argv[0][0] == '-') {
		fold = 0;
		argv[0]++;
		while (*argv[0] >= '0' && *argv[0] <= '9')
			fold =* 10, fold =+ *argv[0]++ - '0';
		if (*argv[0]) {
			printf("Bad number for fold\n");
			flush();
			exit(1);
		}
		argc--, argv++;
	}
	do {
		if (argc > 0) {
			close(ibuf[0]);
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
			putch(c);
		}
	} while (argc > 0);
	flush();
}

int	col;

putch(c)
	register c;
{
	register ncol;

	switch (c) {
		case '\n':
			ncol = 0;
			break;
		case '\t':
			ncol = (col + 8) &~ 7;
			break;
		case '\b':
			ncol = col ? col - 1 : 0;
			break;
		case '\r':
			ncol = 0;
			break;
		default:
			ncol = col + 1;
	}
	if (ncol > fold)
		putchar('\n'), col = 0;
	putchar(c);
	switch (c) {
		case '\n':
			col = 0;
			break;
		case '\t':
			col =+ 8;
			col =& ~7;
			break;
		case '\b':
			if (col)
				col--;
			break;
		case '\r':
			col = 0;
			break;
		default:
			col++;
			break;
	}
}
