/*
 * Copyright (c) 1980 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)df.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * df
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/mount.h>
#include "pathnames.h"

int	iflag;
#ifdef COMPAT_43
int	oflag;
#endif /* COMPAT_43 */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	int ch, i;
	long mntsize;
	struct statfs statfsbuf, *mntbuf;

	while ((ch = getopt(argc, argv, "io")) != EOF)
		switch(ch) {
		case 'i':
			iflag++;
			break;
#ifdef COMPAT_43
		case 'o':
			oflag++;
			break;
#endif /* COMPAT_43 */
		case '?':
		default:
			fprintf(stderr, "usage: df [-i] [filsys ...]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	printf("Filesystem    kbytes    used   avail capacity");
	if (iflag)
		printf(" iused   ifree  %%iused");
	printf("  Mounted on\n");
#ifdef COMPAT_43
	if (oflag) {
		olddf(argv);
		exit(0);
	}
#endif /* COMPAT_43 */
	if (!*argv) {
		if ((mntsize = getfsstat(0, 0)) < 0) {
			perror("df");
			exit(1);
		}
		mntbuf = 0;
		do {
			if (mntbuf)
				free(mntbuf);
			i = (mntsize + 1) * sizeof(struct statfs);
			if ((mntbuf = (struct statfs *)malloc(i)) == 0) {
				fprintf(stderr,
					"no space for mount table buffer\n");
				exit(1);
			}
			if ((mntsize = getfsstat(mntbuf, i)) < 0) {
				perror("df");
				exit(1);
			}
		} while (i == mntsize * sizeof(struct statfs));
		for (i = 0; i < mntsize; i++)
			prtstat(&mntbuf[i]);
		exit(0);
	}
	for (; *argv; argv++) {
		if (statfs(*argv, &statfsbuf) < 0) {
			perror(*argv);
			continue;
		}
		prtstat(&statfsbuf);
	}
	exit(0);
}

/*
 * Print out status about a filesystem.
 */
prtstat(sfsp)
	register struct statfs *sfsp;
{
	long used, availblks, inodes;

	printf("%-12.12s", sfsp->f_mntfromname);
	used = sfsp->f_blocks - sfsp->f_bfree;
	availblks = sfsp->f_bavail + used;
	printf("%8ld%8ld%8ld", sfsp->f_blocks * sfsp->f_fsize / 1024,
	    used * sfsp->f_fsize / 1024, sfsp->f_bavail * sfsp->f_fsize / 1024);
	printf("%6.0f%%",
	    availblks == 0 ? 100.0 : (double)used / (double)availblks * 100.0);
	if (iflag) {
		inodes = sfsp->f_files;
		used = inodes - sfsp->f_ffree;
		printf("%8ld%8ld%6.0f%% ", used, sfsp->f_ffree,
		   inodes == 0 ? 100.0 : (double)used / (double)inodes * 100.0);
	} else 
		printf("  ");
	printf("  %s\n", sfsp->f_mntonname);
}

#ifdef COMPAT_43
/*
 * This code constitutes the old df code for extracting
 * information from filesystem superblocks.
 */
#include <sys/param.h>
#include <ufs/fs.h>
#include <sys/stat.h>
#include <errno.h>
#include <fstab.h>
#include <mtab.h>

struct	mtab mtab[NMOUNT];
char	root[32];
char	*mpath();

union {
	struct fs iu_fs;
	char dummy[SBSIZE];
} sb;
#define sblock sb.iu_fs

int	fi;
char	*strcpy();

olddf(argv)
	char *argv[];
{
	struct fstab *fsp;
	char *strerror();
	int i;

	if ((i = open(_PATH_MTAB, 0)) < 0) {
		fprintf(stderr, "df: %s: %s\n", _PATH_MTAB,
			strerror(errno));
		exit(1);
	}
	(void) read(i, (char *)mtab, sizeof (mtab));
	(void) close(i);
	sync();
	if (!*argv) {
		if (setfsent() == 0)
			perror(_PATH_FSTAB), exit(1);
		while (fsp = getfsent()) {
			if (strcmp(fsp->fs_type, FSTAB_RW) &&
			    strcmp(fsp->fs_type, FSTAB_RO) &&
			    strcmp(fsp->fs_type, FSTAB_RQ))
				continue;
			if (root[0] == 0)
				(void) strcpy(root, fsp->fs_spec);
			dfree(fsp->fs_spec, 1);
		}
		(void)endfsent();
		exit(0);
	}
	while (*argv)
		dfree(*argv++, 0);
	exit(0);
}

dfree(file, infsent)
	char *file;
	int infsent;
{
	extern int errno;
	struct stat stbuf;
	struct statfs statfsbuf;
	register struct statfs *sfsp;
	struct fstab *fsp;
	char *strerror();

	if (stat(file, &stbuf) == 0 &&
	    (stbuf.st_mode&S_IFMT) != S_IFCHR &&
	    (stbuf.st_mode&S_IFMT) != S_IFBLK) {
		if (infsent) {
			fprintf(stderr, "%s: screwy fstab entry\n", file);
			return;
		}
		(void)setfsent();
		while (fsp = getfsent()) {
			struct stat stb;

			if (stat(fsp->fs_spec, &stb) == 0 &&
			    stb.st_rdev == stbuf.st_dev) {
				file = fsp->fs_spec;
				(void)endfsent();
				goto found;
			}
		}
		(void)endfsent();
		fprintf(stderr, "%s: mounted on unknown device\n", file);
		return;
	}
found:
	if ((fi = open(file, 0)) < 0) {
		fprintf(stderr, "df: %s: %s\n", file, strerror(errno));
		return;
	}
	if (bread((long)SBOFF, (char *)&sblock, SBSIZE) == 0) {
		(void) close(fi);
		return;
	}
	sfsp = &statfsbuf;
	sfsp->f_type = MOUNT_UFS;
	sfsp->f_flags = 0;
	sfsp->f_fsize = sblock.fs_fsize;
	sfsp->f_bsize = sblock.fs_bsize;
	sfsp->f_blocks = sblock.fs_dsize;
	sfsp->f_bfree = sblock.fs_cstotal.cs_nbfree * sblock.fs_frag +
		sblock.fs_cstotal.cs_nffree;
	sfsp->f_bavail = (sblock.fs_dsize * (100 - sblock.fs_minfree) / 100) -
		(sblock.fs_dsize - sfsp->f_bfree);
	if (sfsp->f_bavail < 0)
		sfsp->f_bavail = 0;
	sfsp->f_files =  sblock.fs_ncg * sblock.fs_ipg;
	sfsp->f_ffree = sblock.fs_cstotal.cs_nifree;
	sfsp->f_fsid.val[0] = 0;
	sfsp->f_fsid.val[1] = 0;
	bcopy((caddr_t)mpath(file), (caddr_t)&sfsp->f_mntonname[0], MNAMELEN);
	bcopy((caddr_t)file, (caddr_t)&sfsp->f_mntfromname[0], MNAMELEN);
	prtstat(sfsp);
	(void) close(fi);
}

long lseek();

bread(off, buf, cnt)
	long off;
	char *buf;
{
	int n;
	extern errno;

	(void) lseek(fi, off, 0);
	if ((n=read(fi, buf, cnt)) != cnt) {
		/* probably a dismounted disk if errno == EIO */
		if (errno != EIO) {
			printf("\nread error off = %ld\n", off);
			printf("count = %d; errno = %d\n", n, errno);
		}
		return (0);
	}
	return (1);
}

/*
 * Given a name like /dev/rrp0h, returns the mounted path, like /usr.
 */
char *
mpath(file)
	char *file;
{
	register struct mtab *mp;

	if (eq(file, root))
		return ("/");
	for (mp = mtab; mp < mtab + NMOUNT; mp++)
		if (eq(file, mp->m_dname))
			return (mp->m_path);
	return "";
}

eq(f1, f2)
	char *f1, *f2;
{

	if (strncmp(f1, _PATH_DEV, sizeof(_PATH_DEV) - 1) == 0)
		f1 += 5;
	if (strncmp(f2, _PATH_DEV, sizeof(_PATH_DEV) - 1) == 0)
		f2 += 5;
	if (!strcmp(f1, f2))
		return (1);
	if (*f1 == 'r' && !strcmp(f1+1, f2))
		return (1);
	if (*f2 == 'r' && !strcmp(f1, f2+1))
		return (1);
	if (*f1 == 'r' && *f2 == 'r' && strcmp(f1+1, f2+1) == 0)
		return (1);
	return (0);
}
#endif /* COMPAT_43 */
