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

	if (argc < 3) {
		printf("usage: %s file megabytes\n", argv[0]);
		exit(1);
	}
	fd = creat(argv[1], 0644);
	if (fd < 0) {
		perror(argv[1]);
		exit(1);
	}
	fstat(fd, &sb);
	buf = malloc(1024);
	max = (atoi(argv[2]) * 1024 * 1024) / 1024;
	printf("%d writes of %d kilobytes\n", max, 1024 / 1024);
	for (i = 0; i < max; i++)
		write(fd, buf, 1024);
}
