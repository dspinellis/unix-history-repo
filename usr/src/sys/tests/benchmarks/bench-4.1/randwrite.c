/*
 * Random I/O benchmark.
 *
 * Process writes blocks to a
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

	if (argc < 4) {
		printf("usage: %s file max-file-size #writes\n", argv[0]);
		exit(1);
	}
	fd = creat(argv[1], 0644);
	if (fd < 0) {
		perror(argv[1]);
		exit(2);
	}
	buf = malloc(1024);
	if (buf == (char *)0) {
		printf("Couldn't allocate i/o buffer.\n");
		exit(3);
	}
	/* file size is in megabytes */
	fstat(fd, &sb);
	maxblocks = atoi(argv[2]) * ((1024 * 1024) / 1024);
	niter = atoi(argv[3]);
	printf("%d random writes (block size %d)\n", niter, 1024);
	for (i = 0; i < niter; i++) {
		bn = random() % maxblocks;
		lseek(fd, bn * 1024, 0);
		write(fd, buf, 1024);
	}
}
