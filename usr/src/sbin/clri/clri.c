/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rich $alz of BBN Inc.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1990, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)clri.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>

#include <ufs/ufs/dinode.h>
#include <ufs/ffs/fs.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register struct fs *sbp;
	register struct dinode *ip;
	register int fd;
	struct dinode ibuf[MAXBSIZE / sizeof (struct dinode)];
	long generation, bsize;
	off_t offset;
	int inonum;
	char *fs, sblock[SBSIZE];

	if (argc < 3) {
		(void)fprintf(stderr, "usage: clri filesystem inode ...\n");
		exit(1);
	}

	fs = *++argv;

	/* get the superblock. */
	if ((fd = open(fs, O_RDWR, 0)) < 0)
		err(1, "%s", fs);
	if (lseek(fd, (off_t)(SBLOCK * DEV_BSIZE), SEEK_SET) < 0)
		err(1, "%s", fs);
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
			err(1, "%s", fs);
		if (read(fd, ibuf, bsize) != bsize)
			err(1, "%s", fs);

		/* get the inode within the block. */
		ip = &ibuf[itoo(sbp, inonum)];

		/* clear the inode, and bump the generation count. */
		generation = ip->di_gen + 1;
		memset(ip, 0, sizeof(*ip));
		ip->di_gen = generation;

		/* backup and write the block */
		if (lseek(fd, (off_t)-bsize, SEEK_CUR) < 0)
			err(1, "%s", fs);
		if (write(fd, ibuf, bsize) != bsize)
			err(1, "%s", fs);
		(void)fsync(fd);
	}
	(void)close(fd);
	exit(0);
}
