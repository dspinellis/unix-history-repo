/*
 * Random I/O benchmark.
 *
 * Process reads blocks from a
 * file in a random order. 
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

char	*malloc();

main(argc, argv)
	char *argv[];
{
	char *buf;
	int fd, bn, maxblocks;
	struct stat sb;
	register int i, niter;

	if (argc < 3) {
		printf("usage: %s file #reads\n", argv[0]);
		exit(1);
	}
	fd = open(argv[1], 0);
	if (fd < 0) {
		perror(argv[1]);
		exit(2);
	}
	buf = malloc(1024);
	if (buf == (char *)0) {
		printf("Couldn't allocate i/o buffer.\n");
		exit(3);
	}
	fstat(fd, &sb);
	niter = atoi(argv[2]);
	printf("%d random block reads, file size %d kb (bsize %d)\n",
	    niter, sb.st_size / 1024, 1024);
	for (i = 0; i < niter; i++) {
		bn = (random() % sb.st_size) / sb.st_size;
		lseek(fd, bn * 1024, 0);
		read(fd, buf, 1024);
	}
}
