/*
 * pxref - driver to put in /usr/bin/ to call pxref as a Pascal program
 *
 * Bill Joy UCB July 29, 1977
 *
 * This program is not needed if the shell in use understands Pascal
 * objects.
 */

char *progname;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, progname = *argv++;
	if (argc == 0 || argc > 2) {
		printf("usage: %s [ - ] file\n", progname);
		exit(1);
	}
	if (argc == 1) {
		argv--;
		argv[0] = argv[1];
		argv[1] = 0;
	}
	execl("/bin/px", "px", "/usr/lib/pxref", argv[0], argv[1], 0);
	execl("/usr/bin/px", "pcx", "/usr/lib/pxref", argv[0], argv[1], 0);
	write(2, "Can't find px\n", 14);
	exit(1);
}
