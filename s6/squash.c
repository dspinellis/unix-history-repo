/*
 * squash - use the loader to squash unuseful stuff out of object files
 *
 * Bill Joy UCB August 24, 1977
 *
 * This program is faster than a shell script
 * which would serve just as well.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int child, status, i;

	argc--, argv++;
	while (argc > 0) {
		i = fork();
		if (i < 0) {
			write(2, "No more processes\n", 18);
			exit(1);
		}
		if (i == 0) {
			doit("/usr/bin/nld", argv[0]);
			doit("/bin/nld", argv[0]);
			doit("/bin/ld", argv[0]);
			doit("/usr/bin/ld", argv[0]);
			write(2, "Can't find loader\n", 18);
			exit(1);
		}
		do
			child = wait(&status);
		while (child != -1 && child != i);
		if (child == -1) {
			write(2, "Impossible return from wait\n", 28);
			exit(1);
		}
		if (status & 0377) {
			write(2, "Loader process faulted\n", 23);
			unlink("x.out");
			exit(1);
		}
		if (((status >> 8) & 0377) > 1) {
			unlink("x.out");
			exit(1);
		}
		if (unlink(argv[0])) {
			unlink("x.out");
			perror(argv[0]);
			exit(1);
		}
		if (link("x.out", argv[0])) {
			unlink("x.out");
			perror(argv[0]);
			exit(1);
		}
		if (unlink("x.out")) {
			perror("x.out");
			exit(1);
		}
		argc--;
		argv++;
	}
	exit(0);
}

doit(who, what)
	char *who, *what;
{

	execl(who, "ld", "-S", "-X", "-r", "-o", "x.out", what, 0);
}
