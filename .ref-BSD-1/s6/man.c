/*
 * man - intelligent man command
 *
 * Author: Bill Joy UCB August 25, 1977
 *
 * Man is an intelligent man command which obviates the need to know
 * section numbers in the manual.  Also if the standard output is a teletype and
 * the option - is not given we pipe through "ssp" to eliminate piled
 * up blank lines.
 */

extern	int fout;

int	nossp;

main(argc, argv)
	int argc;
	char *argv[];
{
	int i, pvec[2], section, io, tty[3];

	io = open("/usr/adm/manlog", 1);
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
	fout = 2;
	if (chdir("/usr/man") < 0) {
		printf("Can't chdir to /usr/man\n");
		exit(1);
	}
	argc--, argv++;
	if (argv[0][0] == '-') {
		argc--, argv++;
		nossp++;
	} else if (gtty(1, tty))
		nossp++;
	if (argc == 0) {
		printf("Usage: man [ section ] name ...\n");
		exit(1);
	}
	if (nossp == 0) {
		pipe(pvec);
		i = fork();
		if (i != -1) {
			if (i != 0) {
				close(0);
				close(pvec[1]);
				dup(pvec[0]);
				close(pvec[0]);
				execl("/usr/pascal/ssp", "ssp", 0);
				execl("/bin/ssp", "ssp", 0);
				execl("/usr/bin/ssp", "ssp", 0);
				printf("Can't find ssp!\n");
				execl("/bin/cat", "cat", 0);
				printf("or cat - gott in himmel!\n");
				exit(1);
			}
			close(pvec[0]);
			close(1);
			dup(pvec[1]);
			close(pvec[1]);
			fout = 1;
		}
	}
	section = 0;
	do {
		if (argv[0][0] >= '0' && argv[0][0] <= '9' && !argv[0][1]) {
			section = argv[0][0] - '0';
			argc--, argv++;
			if (argc == 0) {
				printf("But what do you want from section %s?\n", argv[-1]);
				exit(1);
			}
			continue;
		}
		manual(section, argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

manual(section, name)
	int section;
	char *name;
{
	char work[100];
	char stbuf[36];
	int last;

	strcpy(work, "manx/");
	strcat(work, name);
	strcat(work, ".x");
	last = strlen(work) - 1;
	if (section == 0) {
		for (section = '1'; section <= '9'; section++) {
			work[3] = section;
			work[last] = section;
			if (stat(work, stbuf) >= 0)
				break;
		}
		if (section > '9') {
			printf("No section in the manual has %s in it\n", name);
			return;
		}
	} else {
		work[3] = section + '0';
		work[last] = section + '0';
		if (stat(work, stbuf) < 0) {
			printf("%s is not in section %d of the manual\n", name, section);
			return;
		}
	}
	nroff(work);
}

nroff(cp)
	char *cp;
{
	int i;

	i = fork();
	if (i < 0) {
		printf("No more processes\n");
		exit(1);
	}
	if (i == 0) {
		donroff("/bin/nroff", cp);
		donroff("/usr/bin/nroff", cp);
		printf("Can't find a nroff - come again\n");
		exit(1);
	}
	while (wait(&i) != -1)
		continue;
}

donroff(nis, what)
	char *nis, *what;
{

	execl(nis, "nroff", "-h", "man0/naa", what, 0);
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
