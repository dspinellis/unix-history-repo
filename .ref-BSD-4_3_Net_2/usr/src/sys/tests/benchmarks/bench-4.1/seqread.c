/*
 * Sequential I/O benchmark.
 */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

char	*malloc();

main(argc, argv)
	char *argv[];
{
	register int i, max;
	char *buf;
	struct stat sb;
	int fd;

	if (argc < 2) {
		printf("usage: %s file\n", argv[0]);
		exit(1);
	}
	fd = open(argv[1], 0);
	if (fd < 0) {
		perror(argv[1]);
		exit(1);
	}
	fstat(fd, &sb);
	buf = malloc(1024);
	while (read(fd, buf, 1024) == 1024)
		;
}
