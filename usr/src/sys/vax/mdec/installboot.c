/*	installboot.c	4.1	83/02/16	*/

#include "../h/param.h"
#include "../h/fs.h"

char bootimage[BBSIZE];

main(argc, argv)
	int argc;
	char *argv[];
{
	int fd;

	if (argc != 4) {
		printf("Usage: installboot bootblock bootprog device\n");
		exit(1);
	}
	fd = open(argv[1], 0);
	if (fd < 0) {
		perror(argv[1]);
		exit(1);
	}
	read(fd, bootimage, DEV_BSIZE);
	close(fd);
	fd = open(argv[2], 0);
	if (fd < 0) {
		perror(argv[2]);
		exit(1);
	}
	read(fd, &bootimage[DEV_BSIZE], BBSIZE - DEV_BSIZE);
	close(fd);
	fd = open(argv[3], 1);
	if (fd < 0) {
		perror(argv[3]);
		exit(1);
	}
	write(fd, bootimage, BBSIZE);
	close(fd);
}
