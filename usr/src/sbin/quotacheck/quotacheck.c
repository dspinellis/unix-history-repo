#ifndef lit
static char sccsid[] = "@(#)quotacheck.c	4.1 (Melbourne) %G%";
#endif

/*
 * Fix up / report on disc quotas & usage
 */
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#define	QUOTA
#include <sys/quota.h>
#include <sys/stat.h>

#define	ITABSZ	256
#define	NUID	3500
union {
	struct	fs	sblk;
	char	___[MAXBSIZE];
} un;
#define	sblock	un.sblk
struct	dinode	itab[ITABSZ];
struct	dinode	*dp;
struct	dqblk	du[NUID];
char	*dn[NUID];
struct	dqblk	zeroes;
u_short	iuse[NUID];
u_long	buse[NUID];
long	blocks;
dev_t	dev;

int	bflg;
int	iflg;
int	rflg;
int	sflg;

int	fi;
unsigned	ino;
unsigned	nfiles;
int	highuid;

struct	passwd	*getpwent();
struct	dinode	*ginode();
char	*malloc();
char	*copy();

main(argc, argv)
	char **argv;
{
	register int n;
	register struct passwd *lp;
	register char *p;
	register long unsigned i;
	register c;
	register cg;
	FILE *qf;
	struct stat statb;

	while (--argc > 0 && *(p = *++argv) == '-')
		while (c = *++p) switch (c) {

		case 's':
			sflg++;
			break;

		case 'b':
			bflg++;
			break;

		case 'i':
			iflg++;
			break;

		case 'r':
			rflg++;
			break;
		}

	if (argc != 2) {
		fprintf(stderr, "Usage: fixquota filesys qfile\n");
		exit(1);
	}

	fi = open(p, 0);
	if (fi < 0) {
		fprintf(stderr, "Can't open %s\n", p);
		exit(1);
	}

	if (iflg || bflg || rflg) {
		while((lp=getpwent()) != 0) {
			n = lp->pw_uid;
			if (n>=NUID)
				continue;
			if(dn[n])
				continue;
			dn[n] = copy(lp->pw_name);
		}
	}

	if (!(iflg || bflg))
		sflg++;

	qf = fopen(*++argv, "r");
	if (qf != NULL) {
		fstat(fileno(qf), &statb);
		dev = statb.st_dev;
		quota(Q_SYNC, 0, dev, 0);
		n = fread(du, sizeof(struct dqblk), NUID, qf);
		if (n == EOF)
			n = 0;
		highuid = n-1;
		fclose(qf);
	} else {
		highuid = -1;
		dev = NODEV;
	}
/*ZZprintf("highuid = %d\n", highuid);*/

	for (n = 0; n <= highuid; n++) {
		iuse[n] = du[n].dqb_curinodes;
		buse[n] = du[n].dqb_curblocks;
		du[n].dqb_curinodes = du[n].dqb_curblocks = 0;
	}

	sync();
	bread(SBLOCK, (char *)&sblock, SBSIZE);
	ino = 0;
	for (cg = 0; cg < sblock.fs_ncg; cg++) {
/*ZZprintf("cg %d <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", cg);*/
		dp = NULL;
		for (i = 0; i < sblock.fs_ipg; i++)
			acct(ginode());
	}
	if (sflg && highuid >= 0) {
		int sig;
		int ssig;

		sig = (int) signal(SIGINT, SIG_IGN);
		ssig = (int) signal(SIGTSTP, SIG_IGN);
		if ((qf = fopen(*argv, "a")) == NULL) {
			fprintf(stderr, "Can't create %s\n", *argv);
			exit(1);
		}

		rewind(qf);
		fwrite(du, sizeof(struct dqblk), highuid+1, qf);
		fclose(qf);
		signal(SIGTSTP, ssig);
		sysset();
		signal(SIGINT, sig);
	}
	report();
}

acct(ip)
	register struct dinode *ip;
{
	register n;

	if (ip == NULL)
		return;
	if (ip->di_mode == 0)
/*ZZ{printf(" unallocated\n");*/
		return;
/*ZZ}*/
	if (ip->di_uid >= NUID)
/*ZZ{printf(" uid oor\n");*/
		return;
/*ZZ}*/
	if (ip->di_uid > highuid) {
		for (n = highuid+1; n <= ip->di_uid; n++)
			du[n] = zeroes;
		highuid = ip->di_uid;
	}
	du[ip->di_uid].dqb_curinodes++;
	if ((ip->di_mode & IFMT) == IFCHR || (ip->di_mode & IFMT) == IFBLK)
/*ZZ{printf(" special\n");*/
		return;
/*ZZ}*/
	blocks = 0;
	for (n = 0; n < NDADDR; n++)
		if (ip->di_db[n])
			blocks += dblksize(&sblock, ip, n) / DEV_BSIZE;
	for (n = 0; n < NIADDR; n++)
		tloop(ip->di_ib[n], ip, n);
	du[ip->di_uid].dqb_curblocks += blocks;
	if (blocks != ip->di_blocks)
		printf("Ino %d: <calc %d, recorded %d>\n", ino, blocks, ip->di_blocks);
/*ZZprintf(" %d blks\n", blocks);*/
}

tloop(bn, ip, f)
	long bn;
{
	register i;
	long	iblk[MAXBSIZE/sizeof(long)];

	if (!bn)
		return;
	blocks += sblock.fs_bsize / DEV_BSIZE;
	bread(fsbtodb(&sblock, bn), iblk, sblock.fs_bsize);
	if (f) {
		for (i = 0; i < NINDIR(&sblock); i++)
			tloop(iblk[i], ip, f-1);
	} else {
		for (i = 0; i < NINDIR(&sblock); i++)
			if (iblk[i])
				blocks += sblock.fs_bsize / DEV_BSIZE;
	}
}

struct dinode *
ginode()
{
	register unsigned long iblk;

	if (dp == NULL || ++dp >= &itab[ITABSZ]) {
		iblk = itod(&sblock, ino);
/*ZZprintf("dp = %x, itab=%x", dp, itab);*/
/*ZZprintf(" Reading inodes from fs blk %d ", iblk);*/
		bread(fsbtodb(&sblock, iblk), (char *)itab, sizeof itab);
		dp = &itab[ino % INOPB(&sblock)];
/*ZZprintf("dp = %x\n", dp, itab);*/
	}
/*ZZprintf("ino %d ", ino);*/
	if (ino++ < ROOTINO)
		return(NULL);
	return(dp);
}

bread(bno, buf, cnt)
	long unsigned bno;
	char *buf;
{

	lseek(fi, (long)bno*DEV_BSIZE, 0);
	if (read(fi, buf, cnt) != cnt) {
		printf("read error %u\n", bno);
		exit(1);
	}
}

sysset()
{
	struct dqusage usage;
	register i;

	for (i = 0; i <= highuid; i++) {
		if (du[i].dqb_curinodes != iuse[i] || du[i].dqb_curblocks != buse[i]) {
			if (du[i].dqb_isoftlimit == 0 && du[i].dqb_bsoftlimit == 0)
				continue;
			if (rflg) {
				if (dn[i])
					printf("%s", dn[i]);
				else
					printf("#%d", i);
				printf(": i %d->%d, b %d->%d\n"
					, iuse[i]
					, du[i].dqb_curinodes
					, buse[i]
					, du[i].dqb_curblocks
				);
			}
			usage.du_curinodes = du[i].dqb_curinodes;
			usage.du_curblocks = du[i].dqb_curblocks;
			quota(Q_SETDUSE, i, dev, &usage);
		}
	}
}

report()
{
	register i;

	if (iflg)
		for (i = 0; i <= highuid; i++)
			if (du[i].dqb_isoftlimit && du[i].dqb_curinodes >= du[i].dqb_isoftlimit) {
				if (dn[i])
					printf("%-10s", dn[i]);
				else
					printf("#%-9d", i);
				printf("%5d (iq = %d)\n", du[i].dqb_curinodes, du[i].dqb_isoftlimit);
			}

	if (bflg)
		for (i = 0; i <= highuid; i++)
			if (du[i].dqb_bsoftlimit && du[i].dqb_curblocks >= du[i].dqb_bsoftlimit) {
				if (dn[i])
					printf("%-10s", dn[i]);
				else
					printf("#%-9s", i);
				printf("%5d (quot = %d)\n", du[i].dqb_curblocks, du[i].dqb_bsoftlimit);
			}
}

char *
copy(s)
	char *s;
{
	register char *p;
	register n;

	for(n=0; s[n]; n++)
		;
	p = malloc((unsigned)n+1);
	for(n=0; p[n] = s[n]; n++)
		;
	return(p);
}
