/*
 * Enable or disable X.25 accounting.
 */

main(argc, argv)
	char **argv;
{
	if (x25acct(argc > 1 ? argv[1] : (char *)0) < 0) {
		perror(argv[0]);
		exit(1);
	}
	exit(0);
}
