/*	@(#)negseek.c	1.3 90/01/08 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * test seek to negative offset
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
	long i;
	char buf[8192];
	extern int errno;
	extern long lseek();

	if (argc != 2) {
		fprintf(stderr, "usage: negseek filename\n");
		exit(1);
	}
	fd = open(argv[1], O_CREAT|O_RDONLY, 0666);
	if (fd == -1) {
		perror(argv[1]);
		exit(1);
	}

	for ( i = 0L; i>-10240L ;i -= 1024L ) {
		if (lseek(fd, i, 0) == -1L) {
			perror("lseek");
			close(fd);
			unlink(argv[1]);
#ifdef SVR3
			exit(0);
#else
			exit(1);
#endif
		}
		if (read(fd, buf, sizeof buf) == -1) {
			perror("read");
			close(fd);
			unlink(argv[1]);
#ifdef SVR3
			exit(1);
#else
			exit(0);
#endif
		}
	}
	close(fd);
	unlink(argv[1]);
	exit(1);
}
