/*
 * Random I/O benchmark.
 *
 * Process writes blocks to a file in a
 * random order.  Writes are constrained
 * to overwrite existing blocks.  This
 * test should be run after the seqio test
 * so that a file is constructed.
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

char	*malloc();

main(argc, argv)
	char *argv[];
{
	char *buf;
	int fd, bn, nblocks;
	struct stat sb;
	register int i, niter;

	if (argc < 3) {
		printf("usage: %s file #writes\n", argv[0]);
		exit(1);
	}
	fd = open(argv[1], 1);
	if (fd < 0) {
		perror(argv[1]);
		exit(2);
	}
	fstat(fd, &sb);
	buf = malloc(1024);
	if (buf == (char *)0) {
		printf("Couldn't allocate i/o buffer.\n");
		exit(3);
	}
	nblocks = sb.st_size / 1024;
	niter = atoi(argv[2]);
	printf("%d random writes in %d block file (block size %d)\n", 
	    niter, nblocks, 1024);
	for (i = 0; i < niter; i++) {
		bn = random() % nblocks;
		lseek(fd, bn * 1024, 0);
		write(fd, buf, 1024);
	}
}
