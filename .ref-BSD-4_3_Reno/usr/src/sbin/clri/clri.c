/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rich $alz of BBN Inc.
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
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)clri.c	5.2 (Berkeley) 6/11/90";
#endif /* not lint */

/*
 * clri(8)
 */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <ufs/quota.h>
#include <ufs/inode.h>
#include <ufs/fs.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

char *fs;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fs *sbp;
	register struct dinode *ip;
	register int fd;
	struct dinode ibuf[MAXBSIZE / sizeof (struct dinode)];
	long generation, offset, bsize;
	int inonum;
	char sblock[SBSIZE];

	if (argc < 3) {
		(void)fprintf(stderr, "usage: clri filesystem inode ...\n");
		exit(1);
	}

	fs = *++argv;

	/* get the superblock. */
	if ((fd = open(fs, O_RDWR, 0)) < 0)
		error();
	if (lseek(fd, SBLOCK * DEV_BSIZE, SEEK_SET) < 0)
		error();
	if (read(fd, sblock, sizeof(sblock)) != sizeof(sblock)) {
		(void)fprintf(stderr,
		    "clri: %s: can't read the superblock.\n", fs);
		exit(1);
	}

	sbp = (struct fs *)sblock;
	if (sbp->fs_magic != FS_MAGIC) {
		(void)fprintf(stderr,
		    "clri: %s: superblock magic number 0x%x, not 0x%x.\n",
		    fs, sbp->fs_magic, FS_MAGIC);
		exit(1);
	}
	bsize = sbp->fs_bsize;

	/* remaining arguments are inode numbers. */
	while (*++argv) {
		/* get the inode number. */
		if ((inonum = atoi(*argv)) <= 0) {
			(void)fprintf(stderr,
			    "clri: %s is not a valid inode number.\n", *argv);
			exit(1);
		}
		(void)printf("clearing %d\n", inonum);

		/* read in the appropriate block. */
		offset = itod(sbp, inonum);	/* inode to fs block */
		offset = fsbtodb(sbp, offset);	/* fs block to disk block */
		offset *= DEV_BSIZE;		/* disk block to disk bytes */

		/* seek and read the block */
		if (lseek(fd, offset, SEEK_SET) < 0)
			error();
		if (read(fd, (char *)ibuf, bsize) != bsize)
			error();

		/* get the inode within the block. */
		ip = &ibuf[itoo(sbp, inonum)];

		/* clear the inode, and bump the generation count. */
		generation = ip->di_gen + 1;
		bzero((char *)ip, sizeof *ip);
		ip->di_gen = generation;

		/* backup and write the block */
		if (lseek(fd, -bsize, SEEK_CUR) < 0)
			error();
		if (write(fd, (char *)ibuf, bsize) != bsize)
			error();
		(void)fsync(fd);
	}
	(void)close(fd);
	exit(0);
}

error()
{
	(void)fprintf(stderr, "clri: %s: %s\n", fs, strerror(errno));
	exit(1);
}
