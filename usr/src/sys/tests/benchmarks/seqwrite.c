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
	fd = open(argv[1], O_WRONLY|O_CREAT|O_TRUNC, 0644);
	if (fd < 0) {
		perror(argv[1]);
		exit(1);
	}
	fstat(fd, &sb);
	buf = malloc(sb.st_blksize);
	max = (atoi(argv[2]) * 1024 * 1024) / sb.st_blksize;
	printf("%d writes of %d kilobytes\n", max, sb.st_blksize / 1024);
	for (i = 0; i < max; i++)
		write(fd, buf, sb.st_blksize);
}
