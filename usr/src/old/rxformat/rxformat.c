/*	rxformat.c	4.2	83/04/28	*/

#include <stdio.h>
#include <sys/file.h>
#include <errno.h>
#include "/sys/vaxuba/rxreg.h"

/*
 * format floppy disks on RX02
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int fd, idens = 0, filarg = 1;

	if (argc < 2)
		usage();
	if (argc == 3) { 
		if (strncmp(argv[1],"-d",2) != 0)
			usage();
		idens++;
		filarg = 2;
	}
	if ((fd = open(argv[filarg], FRDWR, 0666)) < NULL) {
		perror(argv[filarg]);
		exit (0);
	}
	printf("Format %s to", *(argv[filarg]));
	if (idens)
		printf(" double density (y/n) ?");
	else
		printf(" single density (y/n) ?");
	if (getchar() != 'y')
		exit (0);
	if (ioctl(fd, RXIOC_FORMAT, &idens) != NULL)
		perror(argv[2]);
	close (fd);
}

usage()
{
	fprintf(stderr, "usage: rxformat [-d] /dev/rx?\n");
	exit (0);
}
