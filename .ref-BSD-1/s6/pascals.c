/*
 * pcs - C front end for pascals
 *
 * Author: Bill Joy UCB June 1977
 *
 * This program implements Pascals as though it were
 * a real program with a syntax:
 *
 *	pcs program
 *
 * Note that it is too large to be interpreted with a px that
 * does not run in separate i/d space.
 */
char	pcs[]	"/usr/lib/pascals";

main(argc, argv)
	int argc;
	char *argv[];
{
	if (argc != 2) {
		printf("Usage: %s file\n", argv[0]);
		exit (1);
	}
	close(0);
	if (open(argv[1], 0) < 0) {
		perror(argv[1]);
		exit(1);
	}
	execl("/bin/px", "px", pcs, 0);
	execl("/usr/bin/px", "px", pcs, 0);
	write(2, "Can't find px\n", 14);
	exit(1);
}
