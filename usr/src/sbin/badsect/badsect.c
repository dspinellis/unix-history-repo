/*
 * Copyright (c) 1981, 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1981, 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)badsect.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * badsect
 *
 * Badsect takes a list of file-system relative sector numbers
 * and makes files containing the blocks of which these sectors are a part.
 * It can be used to contain sectors which have problems if these sectors
 * are not part of the bad file for the pack (see bad144).  For instance,
 * this program can be used if the driver for the file system in question
 * does not support bad block forwarding.
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/fs.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/inode.h>

union {
	struct	fs fs;
	char	fsx[SBSIZE];
} ufs;
#define sblock	ufs.fs
union {
	struct	cg cg;
	char	cgx[MAXBSIZE];
} ucg;
#define	acg	ucg.cg
struct	fs *fs;
int	fso, fsi;
int	errs;
long	dev_bsize = 1;

char buf[MAXBSIZE];


main(argc, argv)
	int argc;
	char *argv[];
{
	daddr_t number;
	struct stat stbuf, devstat;
	register struct direct *dp;
	DIR *dirp;
	int fd;
	char name[BUFSIZ];

	if (argc < 3) {
		fprintf(stderr, "usage: badsect bbdir blkno [ blkno ]\n");
		exit(1);
	}
	if (chdir(argv[1]) < 0 || stat(".", &stbuf) < 0) {
		perror(argv[1]);
		exit(2);
	}
	strcpy(name, "/dev/");
	if ((dirp = opendir(name)) == NULL) {
		perror(name);
		exit(3);
	}
	while ((dp = readdir(dirp)) != NULL) {
		strcpy(&name[5], dp->d_name);
		if (stat(name, &devstat) < 0) {
			perror(name);
			exit(4);
		}
		if (stbuf.st_dev == devstat.st_rdev &&
		    (devstat.st_mode & IFMT) == IFBLK)
			break;
	}
	closedir(dirp);
	if (dp == NULL) {
		printf("Cannot find dev 0%o corresponding to %s\n",
			stbuf.st_rdev, argv[1]);
		exit(5);
	}
	if ((fsi = open(name, 0)) < 0) {
		perror(name);
		exit(6);
	}
	fs = &sblock;
	rdfs(SBOFF, SBSIZE, (char *)fs);
	dev_bsize = fs->fs_fsize / fsbtodb(fs, 1);
	for (argc -= 2, argv += 2; argc > 0; argc--, argv++) {
		number = atoi(*argv);
		if (chkuse(number, 1))
			continue;
		if (mknod(*argv, IFMT|0600, dbtofsb(fs, number)) < 0) {
			perror(*argv);
			errs++;
		}
	}
	printf("Don't forget to run ``fsck %s''\n", name);
	exit(errs);
}

chkuse(blkno, cnt)
	daddr_t blkno;
	int cnt;
{
	int cg;
	daddr_t fsbn, bn;

	fsbn = dbtofsb(fs, blkno);
	if ((unsigned)(fsbn+cnt) > fs->fs_size) {
		printf("block %d out of range of file system\n", blkno);
		return (1);
	}
	cg = dtog(fs, fsbn);
	if (fsbn < cgdmin(fs, cg)) {
		if (cg == 0 || (fsbn+cnt) > cgsblock(fs, cg)) {
			printf("block %d in non-data area: cannot attach\n",
				blkno);
			return (1);
		}
	} else {
		if ((fsbn+cnt) > cgbase(fs, cg+1)) {
			printf("block %d in non-data area: cannot attach\n",
				blkno);
			return (1);
		}
	}
	rdfs(fsbtodb(fs, cgtod(fs, cg)), (int)sblock.fs_cgsize,
	    (char *)&acg);
	if (!cg_chkmagic(&acg)) {
		fprintf(stderr, "cg %d: bad magic number\n", cg);
		errs++;
		return (1);
	}
	bn = dtogd(fs, fsbn);
	if (isclr(cg_blksfree(&acg), bn))
		printf("Warning: sector %d is in use\n", blkno);
	return (0);
}

/*
 * read a block from the file system
 */
rdfs(bno, size, bf)
	int bno, size;
	char *bf;
{
	int n;

	if (lseek(fsi, bno * dev_bsize, 0) < 0) {
		printf("seek error: %ld\n", bno);
		perror("rdfs");
		exit(1);
	}
	n = read(fsi, bf, size);
	if(n != size) {
		printf("read error: %ld\n", bno);
		perror("rdfs");
		exit(1);
	}
}
