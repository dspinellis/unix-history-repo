/*
 * Benchmark program to calculate vfork+wait
 * overhead (approximately).  Process
 * vvforks and exits while parent waits.
 * The time to run this program is used
 * in calculating exec overhead.
 */

main(argc, argv)
	char *argv[];
{
	register int nvforks, i;
	char *cp;
	int pid, child, status, brksize;

	if (argc < 2) {
		printf("usage: %s number-of-vforks sbrk-size\n", argv[0]);
		exit(1);
	}
	nvforks = atoi(argv[1]);
	if (nvforks < 0) {
		printf("%s: bad number of vforks\n", argv[1]);
		exit(2);
	}
	brksize = atoi(argv[2]);
	if (brksize < 0) {
		printf("%s: bad size to sbrk\n", argv[2]);
		exit(3);
	}
	cp = (char *)sbrk(brksize);
	if ((int)cp == -1) {
		perror("sbrk");
		exit(4);
	}
	for (i = 0; i < brksize; i += 1024)
		cp[i] = i;
	while (nvforks-- > 0) {
		child = vfork();
		if (child == -1) {
			perror("vfork");
			exit(-1);
		}
		if (child == 0)
			_exit(-1);
		while ((pid = wait(&status)) != -1 && pid != child)
			;
	}
	exit(0);
}
