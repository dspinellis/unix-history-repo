/*	@(#)excltest.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * test exclusive create
 */
#include <stdio.h>
#ifdef SVR3
#include <fcntl.h>
#else
#include <sys/file.h>
#endif

main(argc, argv)
	int argc;
	char *argv[];
{
	int fd;

	if (argc > 2) {
		fprintf(stderr, "usage: %s [count]\n", argv[0]);
		exit(1);
	}
	if (argc == 2) {
		int count = atoi(argv[1]);
		for (; count; count--) {
			open("exctest.file", O_CREAT | O_EXCL, 0777);
		}
		exit(0);
	}
	unlink("exctest.file");
	if (open("exctest.file", O_CREAT | O_EXCL, 0777) < 0) {
		perror("exctest.file");
		exit(1);
	}
	if (open("exctest.file", O_CREAT | O_EXCL, 0777) < 0) {
		perror("exctest.file2");
		exit(0);
	}
	exit(0);
}
