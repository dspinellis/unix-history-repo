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
static char sccsid[] = "@(#)df.c	5.19 (Berkeley) %G%";
#endif /* not lint */

/*
 * df
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

char	*getmntpt();
int	iflag, kflag, nflag;
#ifdef COMPAT_43
int	oflag;
#endif /* COMPAT_43 */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	int err, ch, i;
	long width, maxwidth, mntsize, getmntinfo();
	char *mntpt, *mktemp();
	struct stat stbuf;
	struct statfs statfsbuf, *mntbuf;
	struct ufs_args mdev;

	while ((ch = getopt(argc, argv, "ikon")) != EOF)
		switch(ch) {
		case 'i':
			iflag = 1;
			break;
		case 'k':
			kflag = 1;
			break;
		case 'n':
			nflag = 1;
			break;
#ifdef COMPAT_43
		case 'o':
			oflag = 1;
			break;
#endif /* COMPAT_43 */
		case '?':
		default:
			fprintf(stderr,
			    "usage: df [-ikn] [file | file_system ...]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	mntsize = getmntinfo(&mntbuf, MNT_NOWAIT);
	maxwidth = 0;
	for (i = 0; i < mntsize; i++) {
		width = strlen(mntbuf[i].f_mntfromname);
		if (width > maxwidth)
			maxwidth = width;
	}
#ifdef COMPAT_43
	if (oflag) {
		olddf(argv, maxwidth);
		exit(0);
	}
#endif /* COMPAT_43 */
	if (!*argv) {
		mntsize = getmntinfo(&mntbuf, (nflag ? MNT_NOWAIT : MNT_WAIT));
		for (i = 0; i < mntsize; i++)
			prtstat(&mntbuf[i], maxwidth);
		exit(0);
	}
	for (; *argv; argv++) {
		if (stat(*argv, &stbuf) < 0) {
			err = errno;
			if ((mntpt = getmntpt(*argv)) == 0) {
				fprintf(stderr, "df: %s: %s\n", *argv,
				    strerror(err));
				continue;
			}
		} else if ((stbuf.st_mode & S_IFMT) == S_IFBLK) {
			if ((mntpt = getmntpt(*argv)) == 0) {
				mntpt = mktemp("/tmp/df.XXXXXX");
				mdev.fspec = *argv;
				if (!mkdir(mntpt) &&
				    !mount(MOUNT_UFS, mntpt, MNT_RDONLY, &mdev) &&
				    !statfs(mntpt, &statfsbuf)) {
					statfsbuf.f_mntonname[0] = '\0';
					prtstat(&statfsbuf, maxwidth);
				} else
					fprintf(stderr, "df: %s: %s\n",
					    *argv, strerror(errno));
				(void)unmount(mntpt, MNT_NOFORCE);
				(void)rmdir(mntpt);
				continue;
			}
		} else
			mntpt = *argv;
		/*
		 * Statfs does not take a `wait' flag, so we cannot
		 * implement nflag here.
		 */
		if (statfs(mntpt, &statfsbuf) < 0) {
			fprintf(stderr,
			    "df: %s: %s\n", mntpt, strerror(errno));
			continue;
		}
		if (argc == 1)
			maxwidth = strlen(statfsbuf.f_mntfromname) + 1;
		prtstat(&statfsbuf, maxwidth);
	}
	exit(0);
}

char *
getmntpt(name)
	char *name;
{
	long mntsize, i;
	struct statfs *mntbuf;

	mntsize = getmntinfo(&mntbuf, (nflag ? MNT_NOWAIT : MNT_WAIT));
	for (i = 0; i < mntsize; i++) {
		if (!strcmp(mntbuf[i].f_mntfromname, name))
			return (mntbuf[i].f_mntonname);
	}
	return (0);
}

/*
 * Print out status about a filesystem.
 */
prtstat(sfsp, maxwidth)
	register struct statfs *sfsp;
	long maxwidth;
{
	long used, availblks, inodes;
	static int timesthrough;

	if (maxwidth < 11)
		maxwidth = 11;
	if (++timesthrough == 1) {
		printf("%-*.*s%s    used   avail capacity",
		    maxwidth, maxwidth, "Filesystem",
		    kflag ? "  kbytes" : "512-blks");
		if (iflag)
			printf(" iused   ifree  %%iused");
		printf("  Mounted on\n");
	}
	printf("%-*.*s", maxwidth, maxwidth, sfsp->f_mntfromname);
	used = sfsp->f_blocks - sfsp->f_bfree;
	availblks = sfsp->f_bavail + used;
	printf("%8ld%8ld%8ld",
	    sfsp->f_blocks * sfsp->f_fsize / (kflag ? 1024 : 512),
	    used * sfsp->f_fsize / (kflag ? 1024 : 512),
	    sfsp->f_bavail * sfsp->f_fsize / (kflag ? 1024 : 512));
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
#include <ufs/fs.h>
#include <errno.h>
#include <fstab.h>

char	root[MAXPATHLEN];

union {
	struct fs iu_fs;
	char dummy[SBSIZE];
} sb;
#define sblock sb.iu_fs

int	fi;
char	*strcpy();

olddf(argv, maxwidth)
	char *argv[];
	long maxwidth;
{
	struct fstab *fsp;

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
			dfree(fsp->fs_spec, 1, maxwidth);
		}
		(void)endfsent();
		exit(0);
	}
	while (*argv)
		dfree(*argv++, 0, maxwidth);
	exit(0);
}

dfree(file, infsent, maxwidth)
	char *file;
	int infsent;
	long maxwidth;
{
	extern int errno;
	struct stat stbuf;
	struct statfs statfsbuf;
	register struct statfs *sfsp;
	struct fstab *fsp;
	char *mntpt;

	if (stat(file, &stbuf) == 0 &&
	    (stbuf.st_mode&S_IFMT) != S_IFCHR &&
	    (stbuf.st_mode&S_IFMT) != S_IFBLK) {
		if (infsent) {
			fprintf(stderr, "df: %s: screwy fstab entry\n", file);
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
		fprintf(stderr, "df: %s: mounted on unknown device\n", file);
		return;
	}
found:
	if ((fi = open(file, O_RDONLY)) < 0) {
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
	if ((mntpt = getmntpt(file)) == 0)
		mntpt = "";
	bcopy((caddr_t)mntpt, (caddr_t)&sfsp->f_mntonname[0], MNAMELEN);
	bcopy((caddr_t)file, (caddr_t)&sfsp->f_mntfromname[0], MNAMELEN);
	prtstat(sfsp, maxwidth);
	(void) close(fi);
}

long lseek();

bread(off, buf, cnt)
	long off;
	char *buf;
{
	int n;
	extern errno;

	(void) lseek(fi, off, SEEK_SET);
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
#endif /* COMPAT_43 */
