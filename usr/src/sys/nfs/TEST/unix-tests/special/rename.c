/*	@(#)rename.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * rename a file n times
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
	int count;
	int i;
	int fd;

	if (argc != 2) {
		fprintf(stderr, "usage: %s <count>\n", argv[0]);
		exit(1);
	}
	if ((fd = open("rename1", O_CREAT, 0666)) < 0) {
		perror("rename1");
		exit(1);
	}
	close(fd);

	count = atoi(argv[1]);
	for (i=0; i<count; i++) {
		if (rename("rename1", "rename2") < 0) {
			perror("rename rename1 to rename2");
			fprintf(stderr, "%d of %d\n", i, count);
			exit(1);
		}
		if (rename("rename2", "rename1") < 0) {
			perror("rename rename2 to rename1");
			fprintf(stderr, "%d of %d\n", i, count);
			exit(1);
		}
	}
cleanup:
	unlink("rename1");
	unlink("rename2");
	exit(0);
}
