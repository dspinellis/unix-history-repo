/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)df.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/fs.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <fstab.h>
#include <mtab.h>
#include "pathnames.h"

/*
 * df
 */
struct	mtab mtab[NMOUNT];
char	root[32];
char	*mpath();

int	iflag;

union {
	struct fs iu_fs;
	char dummy[SBSIZE];
} sb;
#define sblock sb.iu_fs

int	fi;
daddr_t	alloc();
char	*strcpy();

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	int ch, i;
	char *strerror();

	while ((ch = getopt(argc, argv, "i")) != EOF)
		switch(ch) {
		case 'i':
			iflag++;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: df [-i] [filsys ...]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if ((i = open(_PATH_MTAB, 0)) < 0) {
		fprintf(stderr, "df: %s: %s\n", _PATH_MTAB, strerror(errno));
		exit(1);
	}
	(void) read(i, (char *)mtab, sizeof (mtab));
	(void) close(i);
	sync();
	printf("Filesystem    kbytes    used   avail capacity");
	if (iflag)
		printf(" iused   ifree  %%iused");
	printf("  Mounted on\n");
	if (!*argv) {
		struct fstab *fsp;

		if (setfsent() == 0)
			perror(FSTAB), exit(1);
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
}

dfree(file, infsent)
	char *file;
	int infsent;
{
	extern int errno;
	long totalblks, availblks, avail, free, used;
	struct stat stbuf;
	struct fstab *fsp;
	char *strerror();

	if (stat(file, &stbuf) == 0 &&
	    (stbuf.st_mode&S_IFMT) != S_IFCHR &&
	    (stbuf.st_mode&S_IFMT) != S_IFBLK) {
		if (infsent) {
			fprintf(stderr, "%s: screwy /etc/fstab entry\n", file);
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
	printf("%-12.12s", file);
	totalblks = sblock.fs_dsize;
	free = sblock.fs_cstotal.cs_nbfree * sblock.fs_frag +
	    sblock.fs_cstotal.cs_nffree;
	used = totalblks - free;
	availblks = totalblks * (100 - sblock.fs_minfree) / 100;
	avail = availblks > used ? availblks - used : 0;
	printf("%8ld%8ld%8ld", totalblks * sblock.fs_fsize / 1024,
	    used * sblock.fs_fsize / 1024, avail * sblock.fs_fsize / 1024);
	printf("%6.0f%%",
	    availblks == 0 ? 0.0 : (double) used / (double) availblks * 100.0);
	if (iflag) {
		int inodes = sblock.fs_ncg * sblock.fs_ipg;
		used = inodes - sblock.fs_cstotal.cs_nifree;
		printf("%8ld%8ld%6.0f%% ", used, sblock.fs_cstotal.cs_nifree,
		    inodes == 0 ? 0.0 : (double)used / (double)inodes * 100.0);
	} else 
		printf("  ");
	printf("  %s\n", mpath(file));
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

	if (strncmp(f1, "/dev/", 5) == 0)
		f1 += 5;
	if (strncmp(f2, "/dev/", 5) == 0)
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
