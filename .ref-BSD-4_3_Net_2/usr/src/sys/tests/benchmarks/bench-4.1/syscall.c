/*
 * System call overhead benchmark.
 */
main(argc, argv)
	char *argv[];
{
	register int ncalls;

	if (argc < 2) {
		printf("usage: %s #syscalls\n", argv[0]);
		exit(1);
	}
	ncalls = atoi(argv[1]);
	while (ncalls-- > 0)
		(void) getpid();
}
