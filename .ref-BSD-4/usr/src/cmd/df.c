static	char *sccsid = "@(#)df.c	4.3 (Berkeley) 10/15/80";
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
char root[32];

char *mpath();

daddr_t	blkno	= 1;

int	lflag;
int	iflag;

struct	filsys sblock;

int	fi;
daddr_t	alloc();

main(argc, argv)
char **argv;
{
	int i;
	char buf[128];

	while (argc >= 1 && argv[1][0]=='-') {
		switch(argv[1][1]) {

		case 'l':
			lflag++;
			break;

		case 'i':
			iflag++;
			break;

		default:
			fprintf(stderr, "usage: df [ -il ] [ filsys... ]\n");
			exit(0);
		}
		argc--, argv++;
	}

	if ((i=open("/etc/mtab", 0)) >= 0) {
		read(i, mtab, sizeof mtab);	/* Probably returns short */
		close(i);
	}
	printf("Filesystem  Mounted on  blocks\t  used\t  free");
	if (lflag)
		printf("\thardway");
	printf("\t%% used");
	if (iflag)
		printf("\tiused\tifree\t%%iused");
	putchar('\n');
	if(argc <= 1) {
		struct	fstab	*fsp;
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while( (fsp = getfsent()) != 0){
			if (  (strcmp(fsp->fs_type, FSTAB_RW) != 0)
			    &&(strcmp(fsp->fs_type, FSTAB_RO) != 0) )
				continue;
			if (root[0] == 0)
				strcpy(root, fsp->fs_spec);
			dfree(fsp->fs_spec);
		}
		endfsent();
		exit(0);
	}

	for(i=1; i<argc; i++) {
		dfree(argv[i]);
	}
}

dfree(file)
char *file;
{
	daddr_t i;
	long	blocks;
	long	free;
	long	used;
	long	hardway;
	char	*mp;
	struct	stat stbuf;

	if(stat(file, &stbuf) == 0 && (stbuf.st_mode&S_IFMT) != S_IFCHR
	  && (stbuf.st_mode&S_IFMT) != S_IFBLK) {
		int mt = open("/etc/mtab", 0), len;
		char *str = "/dev/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
		char mtab[32];
		struct stat mstbuf;
		while((len = read(mt, mtab, 32)) == 32) {
			read(mt, &str[5], 32);
			if(stat(str, &mstbuf) == 0 && mstbuf.st_rdev == stbuf.st_dev) {
				file = str;
				break;
			}
		}
		close(mt);
		if(len == 0) {
			fprintf(stderr, "%s: mounted on unknown device\n", file);
			return;
		}
	}
	fi = open(file, 0);
	if(fi < 0) {
		fprintf(stderr,"cannot open %s\n", file);
		return;
	}
	sync();
	bread(1L, (char *)&sblock, sizeof(sblock));
	printf("%-12.12s%s", file, mp = mpath(file));
	if (strlen(mp) < 4)
		putchar('\t');

	blocks = (long) sblock.s_fsize - (long)sblock.s_isize;
	free = sblock.s_tfree;
	used = blocks - free;

	printf("\t%6ld", blocks);
	printf("\t%6ld", used);
	printf("\t%6ld", free);
	if (lflag) {
		hardway = 0;
		while(alloc())
			hardway++;
		printf("\t%6ld", free=hardway);
	}
	printf("\t%5.0f%%", (double) used / (double)blocks * 100.0);
	if (iflag) {
		int inodes = (sblock.s_isize - 2) * INOPB;
		used = inodes - sblock.s_tinode;
		printf("\t%5ld\t%5ld\t%5.0f%%", used, sblock.s_tinode, (double)used/(double)inodes*100.0);
	}
	printf("\n");
	close(fi);
}

daddr_t
alloc()
{
	int i;
	daddr_t b;
	struct fblk buf;

	i = --sblock.s_nfree;
	if(i<0 || i>=NICFREE) {
		printf("bad free count, b=%D\n", blkno);
		return(0);
	}
	b = sblock.s_free[i];
	if(b == 0)
		return(0);
	if(b<sblock.s_isize || b>=sblock.s_fsize) {
		printf("bad free block (%D)\n", b);
		return(0);
	}
	if(sblock.s_nfree <= 0) {
		bread(b, (char *)&buf, sizeof(buf));
		blkno = b;
		sblock.s_nfree = buf.df_nfree;
		for(i=0; i<NICFREE; i++)
			sblock.s_free[i] = buf.df_free[i];
	}
	return(b);
}

bread(bno, buf, cnt)
daddr_t bno;
char *buf;
{
	int n;
	extern errno;

	lseek(fi, bno<<BSHIFT, 0);
	if((n=read(fi, buf, cnt)) != cnt) {
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
			return mtab[i].path;
	return "";
}

eq(f1, f2)
char *f1, *f2;
{
	if (strncmp(f1, "/dev/", 5) == 0)
		f1 += 5;
	if (strncmp(f2, "/dev/", 5) == 0)
		f2 += 5;
	if (strcmp(f1, f2) == 0)
		return 1;
	if (*f1 == 'r' && strcmp(f1+1, f2) == 0)
		return 1;
	if (*f2 == 'r' && strcmp(f1, f2+1) == 0)
		return 1;
	if (*f1 == 'r' && *f2 == 'r' && strcmp(f1+1, f2+1) == 0)
		return 1;
	return 0;
}
