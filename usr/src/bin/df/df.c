static	char *sccsid = "@(#)df.c	4.8 (Berkeley) %G%";

#include <stdio.h>
#include <fstab.h>
#include <sys/param.h>
#include <sys/filsys.h>
#include <sys/fblk.h>
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
daddr_t	blkno	= 1;

int	lflag;
int	iflag;

struct	filsys sblock;

int	fi;
daddr_t	alloc();

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
		fprintf(stderr, "usage: df [ -i ] [ -l ] [ filsys... ]\n");
		exit(0);
	}
	argc--, argv++;
	}
	i = open("/etc/mtab", 0);
	if (i >= 0) {
		read(i, mtab, sizeof mtab);	/* Probably returns short */
		close(i);
	}
	printf("Filesystem  Mounted on      kbytes    used    free");
	printf("   %% used");
	if (iflag)
		printf("   iused   ifree  %%iused");
	putchar('\n');
	if (argc <= 1) {
		struct fstab *fsp;

		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while (fsp = getfsent()) {
			if (!strcmp(fsp->fs_type, FSTAB_RW) &&
			    !(strcmp(fsp->fs_type, FSTAB_RO)))
				continue;
			if (root[0] == 0)
				strcpy(root, fsp->fs_spec);
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
	daddr_t i;
	long blocks, free, used, hardway;
	char *mp;
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
	bread((long) 1, (char *)&sblock, sizeof (sblock));
	printf("%-12.12s%-14.14s", file, mp = mpath(file));
	blocks = (long) sblock.s_fsize - (long)sblock.s_isize;
	free = sblock.s_tfree;
	used = blocks - free;
	printf("%8d%8d%8d", blocks, used, free);
	printf("%8.0f%%", 
	    blocks == 0 ? 0.0 : (double) used / (double)blocks * 100.0);
	if (iflag) {
		int inodes = (sblock.s_isize - 2) * INOPB;
		used = inodes - sblock.s_tinode;
		printf("%8ld%8ld%8.0f%%", used, sblock.s_tinode, 
		    inodes == 0 ? 0.0 : (double)used/(double)inodes*100.0);
	}
	printf("\n");
	close(fi);
}

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
{
	int n;
	extern errno;

	lseek(fi, bno<<BSHIFT, 0);
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
