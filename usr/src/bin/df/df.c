static char *sccsid = "@(#)df.c	4.2 (Berkeley) %G%";
#include <stdio.h>
#include <fstab.h>
#include <sys/param.h>
#include <sys/filsys.h>
#include <sys/fblk.h>

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
	FILE *f = fopen(FSTAB, "r");
	char buf[128];
	struct	fstab	fs;

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
		if (f == NULL)
			perror(FSTAB), exit(1);
		while (!feof(f)){
			fscanf(f, FSTABFMT, FSTABARG(&fs));
			if (strcmp(fs.fs_type, "rw") && strcmp(fs.fs_type, "ro"))
				continue;
			if (root[0] == 0)
				strcpy(root, fs.fs_spec);
			dfree(fs.fs_spec);
		}
		exit(0);
	}

	if (f){
		fscanf(f, FSTABFMT, FSTABARG(&fs));
		strcpy(root, fs.fs_spec);
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
