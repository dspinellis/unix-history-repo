#ifndef lint
static char *sccsid = "@(#)quot.c	4.10 (Berkeley) 85/05/27";
#endif

/*
 * quot
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include <sys/file.h>

#define	ISIZ	(MAXBSIZE/sizeof(struct dinode))
union {
	struct fs u_sblock;
	char dummy[SBSIZE];
} sb_un;
#define sblock sb_un.u_sblock
struct dinode itab[MAXBSIZE/sizeof(struct dinode)];

struct du {
	struct	du *next;
	long	blocks;
	long	blocks30;
	long	blocks60;
	long	blocks90;
	long	nfiles;
	int	uid;
#define	NDU	2048
} du[NDU];
int	ndu;
#define	DUHASH	8209	/* smallest prime >= 4 * NDU */
#define	HASH(u)	((u) % DUHASH)
struct	du *duhash[DUHASH];

#define	TSIZE	500
int	sizes[TSIZE];
long	overflow;

int	nflg;
int	fflg;
int	cflg;
int	vflg;
int	hflg;
long	now;

unsigned	ino;

char	*malloc();
char	*getname();

main(argc, argv)
	int argc;
	char *argv[];
{
	register int n;

	now = time(0);
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		register char *cp;

		for (cp = &argv[0][1]; *cp; cp++)
			switch (*cp) {
			case 'n':
				nflg++; break;
			case 'f':
				fflg++; break;
			case 'c':
				cflg++; break;
			case 'v':
				vflg++; break;
			case 'h':
				hflg++; break;
			default:
				fprintf(stderr,
				    "usage: quot [ -nfcvh ] [ device ... ]\n");
				exit(1);
			}
		argc--, argv++;
	}
	if (argc == 0)
		quotall();
	while (argc-- > 0)
		if (check(*argv++) == 0)
			report();
	exit (0);
}

#include <fstab.h>

quotall()
{
	register struct fstab *fs;
	register char *cp;
	char dev[80], *rindex();

	if (setfsent() == 0) {
		fprintf(stderr, "quot: no %s file\n", FSTAB);
		exit(1);
	}
	while (fs = getfsent()) {
		if (strcmp(fs->fs_type, FSTAB_RO) &&
		    strcmp(fs->fs_type, FSTAB_RW) &&
		    strcmp(fs->fs_type, FSTAB_RQ))
			continue;
		cp = rindex(fs->fs_spec, '/');
		if (cp == 0)
			continue;
		sprintf(dev, "/dev/r%s", cp + 1);
		if (check(dev) == 0)
			report();
	}
	endfsent();
}

check(file)
	char *file;
{
	register int i, j, nfiles;
	register struct du **dp;
	daddr_t iblk;
	int c, fd;

	/*
	 * Initialize tables between checks;
	 * because of the qsort done in report()
	 * the hash tables must be rebuilt each time.
	 */
	for (i = 0; i < TSIZE; i++)
		sizes[i] = 0;
	overflow = 0;
	for (dp = duhash; dp < &duhash[DUHASH]; dp++)
		*dp = 0;
	ndu = 0;
	fd = open(file, O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "quot: ");
		perror(file);
		return (-1);
	}
	printf("%s:\n", file);
	sync();
	bread(fd, SBLOCK, (char *)&sblock, SBSIZE);
	if (nflg) {
		if (isdigit(c = getchar()))
			ungetc(c, stdin);
		else while (c != '\n' && c != EOF)
			c = getchar();
	}
	nfiles = sblock.fs_ipg * sblock.fs_ncg;
	for (ino = 0; ino < nfiles; ) {
		iblk = fsbtodb(&sblock, itod(&sblock, ino));
		bread(fd, iblk, (char *)itab, sblock.fs_bsize);
		for (j = 0; j < INOPB(&sblock) && ino < nfiles; j++, ino++) {
			if (ino < ROOTINO)
				continue;
			acct(&itab[j]);
		}
	}
	close(fd);
}

acct(ip)
	register struct dinode *ip;
{
	register struct du *dp;
	struct du **hp;
	long blks, frags, size;
	char n;
	static fino;

	if ((ip->di_mode & IFMT) == 0)
		return;
	/*
	 * By default, take block count in inode.  Otherwise (-h),
	 * take the size field and estimate the blocks allocated.
	 * The latter does not account for holes in files.
	 */
	if (!hflg)
		size = ip->di_blocks / 2;
	else {
		blks = lblkno(&sblock, ip->di_size);
		frags = blks * sblock.fs_frag +
			numfrags(&sblock, dblksize(&sblock, ip, blks));
		size = frags * sblock.fs_fsize / 1024;
	}
	if (cflg) {
		if ((ip->di_mode&IFMT) != IFDIR && (ip->di_mode&IFMT) != IFREG)
			return;
		if (size >= TSIZE) {
			overflow += size;
			size = TSIZE-1;
		}
		sizes[size]++;
		return;
	}
	hp = &duhash[HASH(ip->di_uid)];
	for (dp = *hp; dp; dp = dp->next)
		if (dp->uid == ip->di_uid)
			break;
	if (dp == 0) {
		if (ndu >= NDU)
			return;
		dp = &du[ndu++];
		dp->next = *hp;
		*hp = dp;
		dp->uid = ip->di_uid;
		dp->nfiles = 0;
		dp->blocks = 0;
		dp->blocks30 = 0;
		dp->blocks60 = 0;
		dp->blocks90 = 0;
	}
	dp->blocks += size;
#define	DAY (60 * 60 * 24)	/* seconds per day */
	if (now - ip->di_atime > 30 * DAY)
		dp->blocks30 += size;
	if (now - ip->di_atime > 60 * DAY)
		dp->blocks60 += size;
	if (now - ip->di_atime > 90 * DAY)
		dp->blocks90 += size;
	dp->nfiles++;
	while (nflg) {
		register char *np;

		if (fino == 0)
			if (scanf("%d", &fino) <= 0)
				return;
		if (fino > ino)
			return;
		if (fino < ino) {
			while ((n = getchar()) != '\n' && n != EOF)
				;
			fino = 0;
			continue;
		}
		if (np = getname(dp->uid))
			printf("%.7s	", np);
		else
			printf("%d	", ip->di_uid);
		while ((n = getchar()) == ' ' || n == '\t')
			;
		putchar(n);
		while (n != EOF && n != '\n') {
			n = getchar();
			putchar(n);
		}
		fino = 0;
		break;
	}
}

bread(fd, bno, buf, cnt)
	unsigned bno;
	char *buf;
{

	lseek(fd, (long)bno * DEV_BSIZE, L_SET);
	if (read(fd, buf, cnt) != cnt) {
		fprintf(stderr, "quot: read error at block %u\n", bno);
		exit(1);
	}
}

qcmp(p1, p2)
	register struct du *p1, *p2;
{
	char *s1, *s2;

	if (p1->blocks > p2->blocks)
		return (-1);
	if (p1->blocks < p2->blocks)
		return (1);
	s1 = getname(p1->uid);
	if (s1 == 0)
		return (0);
	s2 = getname(p2->uid);
	if (s2 == 0)
		return (0);
	return (strcmp(s1, s2));
}

report()
{
	register i;
	register struct du *dp;

	if (nflg)
		return;
	if (cflg) {
		register long t = 0;

		for (i = 0; i < TSIZE - 1; i++)
			if (sizes[i]) {
				t += i*sizes[i];
				printf("%d	%d	%D\n", i, sizes[i], t);
			}
		printf("%d	%d	%D\n",
		    TSIZE - 1, sizes[TSIZE - 1], overflow + t);
		return;
	}
	qsort(du, ndu, sizeof (du[0]), qcmp);
	for (dp = du; dp < &du[ndu]; dp++) {
		register char *cp;

		if (dp->blocks == 0)
			return;
		printf("%5D\t", dp->blocks);
		if (fflg)
			printf("%5D\t", dp->nfiles);
		if (cp = getname(dp->uid))
			printf("%-8.8s", cp);
		else
			printf("#%-8d", dp->uid);
		if (vflg)
			printf("\t%5D\t%5D\t%5D",
			    dp->blocks30, dp->blocks60, dp->blocks90);
		printf("\n");
	}
}

/* rest should be done with nameserver or database */

#include <pwd.h>
#include <grp.h>
#include <utmp.h>

struct	utmp utmp;
#define	NMAX	(sizeof (utmp.ut_name))
#define SCPYN(a, b)	strncpy(a, b, NMAX)

#define NUID	64	/* power of 2 */
#define UIDMASK	0x3f

struct ncache {
	int	uid;
	char	name[NMAX+1];
} nc[NUID];
char	outrangename[NMAX+1];
int	outrangeuid = -1;

char *
getname(uid)
{
	register struct passwd *pw;
	struct passwd *getpwent();
	extern int _pw_stayopen;
	register int cp;

	_pw_stayopen = 1;
	cp = uid & UIDMASK;
	if (uid >= 0 && nc[cp].uid == uid && nc[cp].name[0])
		return (nc[cp].name);
	pw = getpwuid(uid);
	if (!pw)
		return (0);
	nc[cp].uid = uid;
	SCPYN(nc[cp].name, pw->pw_name);
	return (nc[cp].name);
}
