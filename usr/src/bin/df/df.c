/*
 * Copyright (c) 1980, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)df.c	5.31 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int	 bread __P((off_t, void *, int));
char	*getbsize __P((char *, int *, long *));
char	*getmntpt __P((char *));
void	 prtstat __P((struct statfs *, long));
void	 ufs_df __P((char *, long));
void	 usage __P((void));

int	iflag, nflag;
struct	ufs_args mdev;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct stat stbuf;
	struct statfs statfsbuf, *mntbuf;
	long width, maxwidth, mntsize;
	int err, ch, i;
	char *mntpt;

	while ((ch = getopt(argc, argv, "ikn")) != EOF)
		switch(ch) {
		case 'i':
			iflag = 1;
			break;
		case 'k':		/* Delete before 4.4BSD. */
			(void)fprintf(stderr, "df: -k no longer supported\n");
			break;
		case 'n':
			nflag = 1;
			break;
		case '?':
		default:
			usage();
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
		} else if ((stbuf.st_mode & S_IFMT) == S_IFCHR) {
			ufs_df(*argv, maxwidth);
			continue;
		} else if ((stbuf.st_mode & S_IFMT) == S_IFBLK) {
			if ((mntpt = getmntpt(*argv)) == 0) {
				mntpt = mktemp(strdup("/tmp/df.XXXXXX"));
				mdev.fspec = *argv;
				if (mkdir(mntpt, DEFFILEMODE) != 0) {
					fprintf(stderr, "df: %s: %s\n",
					    mntpt, strerror(errno));
					continue;
				}
				if (mount(MOUNT_UFS, mntpt, MNT_RDONLY,
				    &mdev) != 0) {
					ufs_df(*argv, maxwidth);
					(void)rmdir(mntpt);
					continue;
				} else if (statfs(mntpt, &statfsbuf)) {
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
	return (0);
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
void
prtstat(sfsp, maxwidth)
	register struct statfs *sfsp;
	long maxwidth;
{
	static long blocksize;
	static int headerlen, timesthrough;
	static char *header;
	long used, availblks, inodes;

	if (maxwidth < 11)
		maxwidth = 11;
	if (++timesthrough == 1) {
		header = getbsize("df", &headerlen, &blocksize);
		(void)printf("%-*.*s %s    Used   Avail Capacity",
		    maxwidth, maxwidth, "Filesystem", header);
		if (iflag)
			(void)printf(" iused   ifree  %%iused");
		(void)printf("  Mounted on\n");
	}
	(void)printf("%-*.*s", maxwidth, maxwidth, sfsp->f_mntfromname);
	used = sfsp->f_blocks - sfsp->f_bfree;
	availblks = sfsp->f_bavail + used;
	(void)printf(" %*ld %7ld %7ld", headerlen,
	    sfsp->f_blocks * sfsp->f_bsize / blocksize,
	    used * sfsp->f_bsize / blocksize,
	    sfsp->f_bavail * sfsp->f_bsize / blocksize);
	(void)printf(" %5.0f%%",
	    availblks == 0 ? 100.0 : (double)used / (double)availblks * 100.0);
	if (iflag) {
		inodes = sfsp->f_files;
		used = inodes - sfsp->f_ffree;
		(void)printf(" %7ld %7ld %5.0f%% ", used, sfsp->f_ffree,
		   inodes == 0 ? 100.0 : (double)used / (double)inodes * 100.0);
	} else 
		(void)printf("  ");
	(void)printf("  %s\n", sfsp->f_mntonname);
}

/*
 * This code constitutes the pre-system call Berkeley df code for extracting
 * information from filesystem superblocks.
 */
#include <ufs/ffs/fs.h>
#include <errno.h>
#include <fstab.h>

union {
	struct fs iu_fs;
	char dummy[SBSIZE];
} sb;
#define sblock sb.iu_fs

int	rfd;

void
ufs_df(file, maxwidth)
	char *file;
	long maxwidth;
{
	struct statfs statfsbuf;
	register struct statfs *sfsp;
	char *mntpt;
	static int synced;

	if (synced++ == 0)
		sync();

	if ((rfd = open(file, O_RDONLY)) < 0) {
		(void)fprintf(stderr, "df: %s: %s\n", file, strerror(errno));
		return;
	}
	if (bread((off_t)SBOFF, &sblock, SBSIZE) == 0) {
		(void)close(rfd);
		return;
	}
	sfsp = &statfsbuf;
	sfsp->f_type = MOUNT_UFS;
	sfsp->f_flags = 0;
	sfsp->f_bsize = sblock.fs_fsize;
	sfsp->f_iosize = sblock.fs_bsize;
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
	(void)close(rfd);
}

int
bread(off, buf, cnt)
	off_t off;
	void *buf;
	int cnt;
{
	int nr;

	(void)lseek(rfd, off, SEEK_SET);
	if ((nr = read(rfd, buf, cnt)) != cnt) {
		/* Probably a dismounted disk if errno == EIO. */
		if (errno != EIO)
			(void)fprintf(stderr, "\ndf: %qd: %s\n",
			    off, strerror(nr > 0 ? EIO : errno));
		return (0);
	}
	return (1);
}

void
usage()
{
	(void)fprintf(stderr, "usage: df [-in] [file | file_system ...]\n");
	exit(1);
}
