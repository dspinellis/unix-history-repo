/*
 * Sequential I/O benchmark.
 *
 * Overwrites existing data in a file.
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
	fd = open(argv[1], 1);
	if (fd < 0) {
		perror(argv[1]);
		exit(1);
	}
	fstat(fd, &sb);
	buf = malloc(1024);
	max = sb.st_size / 1024;
	printf("%d writes of %d kilobytes\n", max, 1024 / 1024);
	for (i = 0; i < max; i++)
		write(fd, buf, 1024);
}
