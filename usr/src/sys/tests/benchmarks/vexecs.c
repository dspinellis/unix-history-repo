/*
 * Benchmark program to calculate exec
 * overhead (approximately).  Process
 * vforks and execs "null" test program.
 * The time to run the vfork program should
 * then be deducted from this one to
 * estimate the overhead for the exec.
 */

main(argc, argv)
	char *argv[];
{
	register int nexecs, i;
	char *cp, *sbrk();
	int pid, child, status, brksize;

	if (argc < 3) {
		printf("usage: %s number-of-execs sbrk-size job-name\n",
		    argv[0]);
		exit(1);
	}
	nexecs = atoi(argv[1]);
	if (nexecs < 0) {
		printf("%s: bad number of execs\n", argv[1]);
		exit(2);
	}
	brksize = atoi(argv[2]);
	if (brksize < 0) {
		printf("%s: bad size to sbrk\n", argv[2]);
		exit(3);
	}
	cp = sbrk();
	if ((int)cp == -1) {
		perror("sbrk");
		exit(4);
	}
	for (i = 0; i < brksize; i += 1024)
		cp[i] = i;
	while (nexecs-- > 0) {
		child = vfork();
		if (child == -1) {
			perror("vfork");
			exit(-1);
		}
		if (child == 0) {
			execv(argv[3], argv);
			perror("execv");
			_exit(-1);
		}
		while ((pid = wait(&status)) != -1 && pid != child)
			;
	}
	exit(0);
}
