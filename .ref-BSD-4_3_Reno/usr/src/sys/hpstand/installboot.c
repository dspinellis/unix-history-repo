/*
 * Copyright (c) 1980, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)installboot.c	7.1 (Berkeley) 5/8/90
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1986, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)installboot.c	7.1 (Berkeley) 5/8/90";
#endif /* not lint */

#include "../sys/param.h"
#include "../ufs/fs.h"

char block[1024];
int maxbootsize = 16 * 7 * 512;		/* XXX */

/*
 * HPs are a kludge.
 * The boot program won't fit in the boot area of a file system so we
 * have to place it outside the file systems.  By convention, this means
 * that if the 'a' partition is being used for '/', it is offset one
 * cylinder into the disk and the boot program goes into that one cylinder.
 * Also by convention, the 'c' partition is defined to be the entire disk
 * including this boot cylinder.  If these conventions are not adhered to,
 * the potential for disaster is enormous.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int ifd, ofd, len;
	char *dev, *standalonecode;
	int bsize = 1024;

	if (argc != 3)
		usage();
	dev = argv[2];
	len = strlen(dev);
	if (dev[len-1] != 'c')
		usage();
	standalonecode = argv[1];
	ifd = open(standalonecode, 0);
	if (ifd < 0) {
		perror(standalonecode);
		exit(1);
	}
	ofd = open(dev, 1);
	if (ofd < 0) {
		perror(dev);
		exit(1);
	}
	while ((len = read(ifd, block, bsize)) > 0) {
		if ((maxbootsize -= bsize) < 0) {
			printf("%s: too big\n", standalonecode);
			exit(2);
		}
		if (len < bsize)
			bzero(&block[len], bsize-len);
		if (write(ofd, block, bsize) != bsize) {
			perror(dev);
			exit(2);
		}
	}
	if (len < 0) {
		perror(standalonecode);
		exit(2);
	}
	exit(0);
}

usage()
{
	printf("Usage: installboot bootprog device\n");
	printf("where:\n");
	printf("\t\"bootprog\" is a LIF format file < %d bytes long\n",
	       maxbootsize);
	printf("\t\"device\" must be the 'c' partition of a bootable disk\n");
	printf("WARNING!!  If the 'c' partition contains a file system, %s\n",
	       "DON'T RUN THIS!!");
	exit(1);
}
