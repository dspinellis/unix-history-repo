/*
 * manwhere - where is something in the manual
 *
 * Author: Bill Joy UCB August 29, 1977
 *
 * Manwhere tells which sections of the manual a given program is in.
 * If given the magic argument '-' it prints only ones which arent in
 * the manual.
 */

extern	int fout;

int	only;

main(argc, argv)
	int argc;
	char *argv[];
{
	int i, pvec[2], section, io, tty[3];

	io = open("/usr/adm/manwherelog", 1);
	if (io > 0 && fork() == 0) {
		char buf[100];
		register char *cp;

		fout = io;
		seek(io, 0, 2);
		if (getpw(getuid(), buf))
			exit(1);
		for (cp = buf; *cp && *cp != ':'; cp++)
			continue;
		*cp = 0;
		printf("%c %-8s  man", ttyn(1), buf);
		for (i = 1; i <= argc; i++)
			printf(" %s", argv[i]);
		putchar('\n');
		flush();
		exit(0);
	}
	close(io);
	if (chdir("/usr/man") < 0) {
		fout = 2;
		printf("Can't chdir to /usr/man\n");
		exit(1);
	}
	argc--, argv++;
	if (argv[0][0] == '-') {
		argc--, argv++;
		only++;
	}
	if (argc == 0) {
		fout = 2;
		printf("Usage: manwhere [ - ] name ...\n");
		exit(1);
	}
	section = 0;
	do {
		manwhere(argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

int	found;

manwhere(name)
	char *name;
{

	found = 0;
	lookfor("manx/", name, "", 1);
	lookfor("/mnt/pascal/man6/", name, "P", 0);
	switch (only) {
		case 0:
			if (found == 0)
				printf("%-14s\t\tNO ENTRY", name);
			putchar('\n');
			break;
		case 1:
			if (found == 0)
				printf("%s\n", name);
			break;
	}
}

lookfor(path, name, prefix, change)
	char *path, *name, *prefix;
	int change;
{
	char work[100];
	char stbuf[36];
	int xaddr, last, section;

	strcpy(work, path);
	xaddr = strlen(path)  - 2;
	strcat(work, name);
	strcat(work, ".x");
	last = strlen(work) - 1;
	for (section = '1'; section <= '9'; section++) {
		if (change)
			work[xaddr] = section;
		work[last] = section;
		if (stat(work, stbuf) >= 0) {
			found++;
			if (!only) {
				if (found == 1)
					printf("%-14s", name);
				printf(" %s%c", prefix, section);
			}
		}
	}
}

strcpy(to, from)
	register char *to, *from;
{

	while (*to++ = *from++)
		continue;
}

strcat(after, with)
	register char *after, *with;
{

	while (*after)
		after++;
	strcpy(after, with);
}
