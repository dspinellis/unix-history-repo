/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rxformat.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/file.h>
#include <vaxuba/rxreg.h>
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
