static char sccsid[] = "@(#)clri.c 2.4 %G%";

/* static char *sccsid = "@(#)clri.c	4.1 (Berkeley) 10/1/80"; */

/*
 * clri filsys inumber ...
 */

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>

#define ISIZE	(sizeof(struct dinode))
#define	NI	(MAXBSIZE/ISIZE)
struct	ino {
	char	junk[ISIZE];
};
struct	ino	buf[NI];

union {
	char		dummy[SBSIZE];
	struct fs	sblk;
} sb_un;
#define sblock sb_un.sblk

int	status;
long	dev_bsize = 1;

main(argc, argv)
	int argc;
	char *argv[];
{
	register i, f;
	unsigned n;
	int j, k;
	long off;

	if (argc < 3) {
		printf("usage: clri filsys inumber ...\n");
		exit(4);
	}
	f = open(argv[1], 2);
	if (f < 0) {
		printf("cannot open %s\n", argv[1]);
		exit(4);
	}
	lseek(f, SBOFF, 0);
	if (read(f, &sblock, SBSIZE) != SBSIZE) {
		printf("cannot read %s\n", argv[1]);
		exit(4);
	}
	if (sblock.fs_magic != FS_MAGIC) {
		printf("bad super block magic number\n");
		exit(4);
	}
	dev_bsize = sblock.fs_fsize / fsbtodb(&sblock, 1);
	for (i = 2; i < argc; i++) {
		if (!isnumber(argv[i])) {
			printf("%s: is not a number\n", argv[i]);
			status = 1;
			continue;
		}
		n = atoi(argv[i]);
		if (n == 0) {
			printf("%s: is zero\n", argv[i]);
			status = 1;
			continue;
		}
		off = fsbtodb(&sblock, itod(&sblock, n)) * dev_bsize;
		lseek(f, off, 0);
		if (read(f, (char *)buf, sblock.fs_bsize) != sblock.fs_bsize) {
			printf("%s: read error\n", argv[i]);
			status = 1;
		}
	}
	if (status)
		exit(status);
	for (i = 2; i < argc; i++) {
		n = atoi(argv[i]);
		printf("clearing %u\n", n);
		off = fsbtodb(&sblock, itod(&sblock, n)) * dev_bsize;
		lseek(f, off, 0);
		read(f, (char *)buf, sblock.fs_bsize);
		j = itoo(&sblock, n);
		for (k = 0; k < ISIZE; k++)
			buf[j].junk[k] = 0;
		lseek(f, off, 0);
		write(f, (char *)buf, sblock.fs_bsize);
	}
	exit(status);
}

isnumber(s)
	char *s;
{
	register c;

	while(c = *s++)
		if (c < '0' || c > '9')
			return(0);
	return(1);
}
