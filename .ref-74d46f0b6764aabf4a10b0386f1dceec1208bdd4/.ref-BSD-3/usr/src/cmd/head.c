#include <stdio.h>
/*
 * head - give the first few lines of a stream or of each of a set of files
 *
 * Bill Joy UCB August 24, 1977
 */

int	linecnt	= 10;
int	argc;

main(Argc, argv)
	int Argc;
	char *argv[];
{
	register int argc;
	char *name;
	register char *argp;
	static int around;
	char obuf[BUFSIZ];

	setbuf(stdout, obuf);
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
			if (freopen(argv[0], "r", stdin) == NULL) {
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
		fflush(stdout);
	} while (argc > 0);
}

copyout(cnt)
	register int cnt;
{
	register int c;
	char lbuf[BUFSIZ];

	while (cnt > 0 && fgets(lbuf, sizeof lbuf, stdin) != 0) {
		printf("%s", lbuf);
		fflush(stdout);
		cnt--;
	}
}

getnum(cp)
	register char *cp;
{
	register int i;

	for (i = 0; *cp >= '0' && *cp <= '9'; cp++)
		i *= 10, i += *cp - '0';
	if (*cp) {
		fprintf(stderr, "Badly formed number\n");
		exit(1);
	}
	return (i);
}
