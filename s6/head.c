#
/*
 * head - give the first few lines of a stream or of each of a set of files
 *
 * Bill Joy UCB August 24, 1977
 */

int	linecnt	10;

int	ibuf[259];

extern	int fout;

main(Argc, argv)
	int Argc;
	char *argv[];
{
	register int argc;
	char *name;
	register char *argp;
	static char around;

	Argc--, argv++;
	argc = Argc;
	do {
		while (argc > 0 && argv[0][0] == '-') {
			linecnt = getnum(argv[0] + 1);
			argc--, argv++, Argc--;
		}
		if (argc == 0 && around)
			break;
		if (argc > 0) {
			close(0);
			if (fopen(argv[0], ibuf) < 0) {
				perror(argv[0]);
				exit(1);
			}
			name = argv[0];
			argc--, argv++;
		} else
			name = 0;
		if (around)
			putchar('\n');
		around++;
		if (Argc > 1 && name)
			printf("==> %s <==\n", name);
		copyout(linecnt);
		flush();
	} while (argc > 0);
}

copyout(cnt)
	register int cnt;
{
	register int c;

	while (cnt > 0)
		for (;;) {
			c = getc(ibuf);
			if (c == -1)
				return;
			putchar(c);
			if (c == '\n') {
				cnt--;
				break;
			}
		}
}

getnum(cp)
	register char *cp;
{
	register int i;

	for (i = 0; *cp >= '0' && *cp <= '9'; cp++)
		i =* 10, i =+ *cp - '0';
	if (*cp) {
		write(2, "Badly formed number\n", 20);
		exit(1);
	}
	return (i);
}
