#include <stdio.h>

int tocrc;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	if (!strcmp(argv[0], "-t"))
		argc--, argv++, tocrc++;
	while (argc > 0)
		getfrom(*argv++), argc--;
	exit(0);
}

getfrom(name)
	char *name;
{
	char headbuf[BUFSIZ];
	char linbuf[BUFSIZ];
	register char *cp;
	int i = 0;

	if (freopen(name, "r", stdin) == 0) {
		perror(name);
		exit(1);
	}
	for (;;) {
		if (fgets(headbuf, sizeof headbuf, stdin) == NULL)
			return;
		if (headbuf[0] != '.')
			continue;
		if (headbuf[1] == 'T' && headbuf[2] == 'H')
			break;
		if (headbuf[1] == 't' && headbuf[2] == 'h')
			break;
	}
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			return;
		if (linbuf[0] != '.')
			continue;
		if (linbuf[1] == 'S' && linbuf[2] == 'H')
			break;
		if (linbuf[1] == 's' && linbuf[2] == 'h')
			break;
	}
	trimln(headbuf);
	if (tocrc) {
		register char *dp = name, *ep;

again:
		while (*dp && *dp != '.')
			putchar(*dp++);
		if (*dp)
			for (ep = dp+1; *ep; ep++)
				if (*ep == '.') {
					putchar(*dp++);
					goto again;
				}
		putchar('(');
		if (*dp)
			dp++;
		while (*dp)
			putchar (*dp++);
		putchar(')');
		putchar(' ');
	}
	printf("%s\t", headbuf);
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			break;
		if (linbuf[0] == '.') {
			if (linbuf[1] == 'S' && linbuf[2] == 'H')
				break;
			if (linbuf[1] == 's' && linbuf[2] == 'h')
				break;
		}
		trimln(linbuf);
		if (i != 0)
			printf(" ");
		i++;
		printf("%s", linbuf);
	}
	printf("\n");
}

trimln(cp)
	register char *cp;
{

	while (*cp)
		cp++;
	if (*--cp == '\n')
		*cp = 0;
}
