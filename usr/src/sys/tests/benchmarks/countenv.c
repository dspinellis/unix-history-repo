/*
 * Benchmark utility to sum up
 * the total number of bytes passed
 * in the environment on an exec.
 */

main(argc, argv, envp)
	char *argv[], *envp[];
{
	register char **p;
	register int n;

	n = 0;
	for (p = envp; *p && *p != (char *)-1; p++)
		n += strlen(*p);
	printf("%d bytes\n", n);
	exit(0);
}
