/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rxformat.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/file.h>
#include <vax/uba/rxreg.h>
#include <stdio.h>
#include <errno.h>
#include "pathnames.h"

char devname[] = _PATH_DEVNAME;

/*
 * Format RX02 floppy disks.
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	int fd, idens = 0, filarg = 1;
	int i, c;

	if (argc < 2 || argc > 3)
		usage();
	if (argc == 3) { 
		if (strncmp(argv[1],"-d",2) != 0)
			usage();
		idens++;
		filarg++;
	}
	devname[8] = argv[filarg][7];
	if ((fd = open(devname, O_RDWR)) < 0) {
		perror(devname);
		exit(1);
	}
	if (isatty(fileno(stdin))) {
		printf("Format %s to %s density (y/n)? ",
			argv[filarg], idens ? "double" : "single");
		i = c = getchar();
		while (c != '\n' && c != EOF)
			c = getchar();
		if (i != 'y')
			exit(0);
	} else
		printf("Formatting %s to %s density\n",
			argv[filarg], idens ? "double" : "single");
	/* 
	 * Change the ioctl command when dkio.h has
	 * been finished.
	 */
	if (ioctl(fd, RXIOC_FORMAT, &idens) == 0)
		exit(0);
	else {
		perror(devname);
		exit(1);
	}
}

usage()
{
	fprintf(stderr, "usage: rxformat [-d] /dev/rx?\n");
	exit(1);
}
