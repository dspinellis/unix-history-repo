#ifndef lint
static	char *sccsid = "@(#)df.c	4.13 %G%";
#endif

#include <stdio.h>
#include <fstab.h>
#include <sys/param.h>
#include <sys/fs.h>
#include <sys/stat.h>

/*
 * df
 */
#define NFS	20	/* Max number of filesystems */

struct {
	char path[32];
	char spec[32];
} mtab[NFS];
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
	int i;

	while (argc >= 1 && argv[1][0]=='-') {
	switch (argv[1][1]) {

	case 'i':
		iflag++;
		break;

	default:
		fprintf(stderr, "usage: df [ -i ] [ filsys... ]\n");
		exit(0);
	}
	argc--, argv++;
	}
	i = open("/etc/mtab", 0);
	if (i >= 0) {
		(void) read(i, (char *)mtab, sizeof mtab);
		(void) close(i);
	}
	sync();
	printf("Filesystem    kbytes    used   avail capacity");
	if (iflag)
		printf(" iused   ifree  %%iused");
	printf("  Mounted on\n");
	if (argc <= 1) {
		struct fstab *fsp;

		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while (fsp = getfsent()) {
			if (!strcmp(fsp->fs_type, FSTAB_RW) &&
			    !(strcmp(fsp->fs_type, FSTAB_RO)))
				continue;
			if (root[0] == 0)
				(void) strcpy(root, fsp->fs_spec);
			dfree(fsp->fs_spec, 1);
		}
		endfsent();
		exit(0);
	}
	for (i=1; i<argc; i++)
		dfree(argv[i], 0);
}

dfree(file, infsent)
	char *file;
	int infsent;
{
	long totalblks, availblks, avail, free, used;
	struct stat stbuf;
	struct fstab *fsp;

	if (stat(file, &stbuf) == 0 &&
	    (stbuf.st_mode&S_IFMT) != S_IFCHR &&
	    (stbuf.st_mode&S_IFMT) != S_IFBLK) {
		if (infsent) {
			fprintf(stderr, "%s: screwy /etc/fstab entry\n", file);
			return;
		}
		setfsent();
		while (fsp = getfsent()) {
			struct stat stb;

			if (stat(fsp->fs_spec, &stb) == 0 &&
			    stb.st_rdev == stbuf.st_dev) {
				file = fsp->fs_spec;
				endfsent();
				goto found;
			}
		}
		endfsent();
		fprintf(stderr, "%s: mounted on unknown device\n", file);
		return;
	}
found:
	fi = open(file, 0);
	if (fi < 0) {
		perror(file);
		return;
	}
	bread(SBLOCK, (char *)&sblock, SBSIZE);
	printf("%-12.12s", file);
	totalblks = sblock.fs_dsize;
	free = sblock.fs_cstotal.cs_nbfree * sblock.fs_frag +
	    sblock.fs_cstotal.cs_nffree;
	used = totalblks - free;
	availblks = totalblks * (100 - sblock.fs_minfree) / 100;
	avail = availblks > used ? availblks - used : 0;
	printf("%8d%8d%8d", totalblks * sblock.fs_fsize / 1024,
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

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
{
	int n;
	extern errno;

	(void) lseek(fi, (long)(bno * DEV_BSIZE), 0);
	if ((n=read(fi, buf, cnt)) != cnt) {
		printf("\nread error bno = %ld\n", bno);
		printf("count = %d; errno = %d\n", n, errno);
		exit(0);
	}
}

/*
 * Given a name like /dev/rrp0h, returns the mounted path, like /usr.
 */
char *mpath(file)
	char *file;
{
	register int i;

	if (eq(file, root))
		return "/";
	for (i=0; i<NFS; i++)
		if (eq(file, mtab[i].spec))
			return (mtab[i].path);
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
