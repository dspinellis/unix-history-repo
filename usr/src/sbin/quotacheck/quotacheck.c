/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)quotacheck.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Fix up / report on disc quotas & usage
 */
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include <sys/quota.h>
#include <sys/stat.h>
#include <fstab.h>
#include <pwd.h>

union {
	struct	fs	sblk;
	char	dummy[MAXBSIZE];
} un;
#define	sblock	un.sblk

#define	ITABSZ	256
struct	dinode	itab[ITABSZ];
struct	dinode	*dp;
long	blocks;
dev_t	dev;

#define LOGINNAMESIZE 8
struct fileusage {
	struct fileusage *fu_next;
	struct dqusage fu_usage;
	u_short	fu_uid;
	char fu_name[LOGINNAMESIZE + 1];
};
#define FUHASH 997
struct fileusage *fuhead[FUHASH];
struct fileusage *lookup();
struct fileusage *adduid();
int highuid;

int fi;
ino_t ino;
long done;
struct	passwd	*getpwent();
struct	dinode	*ginode();
char *malloc(), *makerawname();

int	vflag;		/* verbose */
int	aflag;		/* all file systems */

char *qfname = "quotas";
char quotafile[MAXPATHLEN + 1];
struct dqblk zerodqbuf;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	register struct fileusage *fup;
	register struct passwd *pw;
	int i, errs = 0;

again:
	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-v") == 0) {
		vflag++;
		goto again;
	}
	if (argc > 0 && strcmp(*argv, "-a") == 0) {
		aflag++;
		goto again;
	}
	if (argc <= 0 && !aflag) {
		fprintf(stderr, "Usage:\n\t%s\n\t%s\n",
			"quotacheck [-v] -a",
			"quotacheck [-v] filesys ...");
		exit(1);
	}
	if (vflag) {
		setpwent();
		while ((pw = getpwent()) != 0) {
			fup = lookup(pw->pw_uid);
			if (fup == 0)
				fup = adduid(pw->pw_uid);
			strncpy(fup->fu_name, pw->pw_name,
				sizeof(fup->fu_name));
		}
		endpwent();
	}
	setfsent();
	while ((fs = getfsent()) != NULL) {
		if (aflag &&
		    (fs->fs_type == 0 || strcmp(fs->fs_type, "rq") != 0))
			continue;
		if (!aflag &&
		    !(oneof(fs->fs_file, argv, argc) ||
		      oneof(fs->fs_spec, argv, argc)))
			continue;
		(void) sprintf(quotafile, "%s/%s", fs->fs_file, qfname);
		errs += chkquota(fs->fs_spec, quotafile);
	}
	endfsent();
	for (i = 0; i < argc; i++)
		if ((done & (1 << i)) == 0)
			fprintf(stderr, "%s not found in /etc/fstab\n",
				argv[i]);
	exit(errs);
}

chkquota(fsdev, qffile)
	char *fsdev;
	char *qffile;
{
	register struct fileusage *fup;
	dev_t quotadev;
	FILE *qf;
	u_short uid;
	int cg, i;
	char *rawdisk;
	struct stat statb;
	struct dqblk dqbuf;
	static int warned = 0;
	extern int errno;

	rawdisk = makerawname(fsdev);
	if (vflag)
		fprintf(stdout, "*** Check quotas for %s\n", rawdisk);
	fi = open(rawdisk, 0);
	if (fi < 0) {
		perror(rawdisk);
		return (1);
	}
	qf = fopen(qffile, "r+");
	if (qf == NULL) {
		perror(qffile);
		return (1);
	}
	if (fstat(fileno(qf), &statb) < 0) {
		perror(qffile);
		return (1);
	}
	quotadev = statb.st_dev;
	if (stat(fsdev, &statb) < 0) {
		perror(fsdev);
		return (1);
	}
	if (quotadev != statb.st_rdev) {
		fprintf(stderr, "%s dev (0x%x) mismatch %s dev (0x%x)\n",
			qffile, quotadev, fsdev, statb.st_rdev);
		return (1);
	}
	if (quota(Q_SYNC, 0, quotadev, 0) < 0 &&
	    errno == EINVAL && !warned && vflag) {
		warned++;
		fprintf(stdout,
		    "*** Warning: Quotas are not compiled into this kernel\n");
	}
	sync();
	bread(SBLOCK, (char *)&sblock, SBSIZE);
	ino = 0;
	for (cg = 0; cg < sblock.fs_ncg; cg++) {
		dp = NULL;
		for (i = 0; i < sblock.fs_ipg; i++)
			acct(ginode());
	}
	for (uid = 0; uid <= highuid; uid++) {
		i = fread(&dqbuf, sizeof(struct dqblk), 1, qf);
		if (i == 0)
			dqbuf = zerodqbuf;
		fup = lookup(uid);
		if (fup == 0) {
			if ((dqbuf.dqb_curinodes != 0 ||
			    dqbuf.dqb_curblocks != 0) &&
			    !feof(qf)) {
				dqbuf.dqb_curinodes = 0;
				dqbuf.dqb_curblocks = 0;
				fseek(qf, uid * sizeof(struct dqblk), 0);
				fwrite(&dqbuf, sizeof(struct dqblk), 1, qf);
				fseek(qf, (uid + 1) * sizeof(struct dqblk), 0);
			}
			continue;
		}
		if (dqbuf.dqb_curinodes == fup->fu_usage.du_curinodes &&
		    dqbuf.dqb_curblocks == fup->fu_usage.du_curblocks) {
			fup->fu_usage.du_curinodes = 0;
			fup->fu_usage.du_curblocks = 0;
			continue;
		}
		if (vflag) {
			if (fup->fu_name[0] != '\0')
				printf("%-10s fixed:", fup->fu_name);
			else
				printf("#%-9d fixed:", uid);
			fprintf(stdout, " inodes (old %d, new %d)",
			    dqbuf.dqb_curinodes, fup->fu_usage.du_curinodes);
			fprintf(stdout, " blocks (old %d, new %d)\n",
			    dqbuf.dqb_curblocks, fup->fu_usage.du_curblocks);
		}
		dqbuf.dqb_curinodes = fup->fu_usage.du_curinodes;
		dqbuf.dqb_curblocks = fup->fu_usage.du_curblocks;
		fseek(qf, uid * sizeof(struct dqblk), 0);
		fwrite(&dqbuf, sizeof(struct dqblk), 1, qf);
		fseek(qf, (uid + 1) * sizeof(struct dqblk), 0);
		quota(Q_SETDUSE, uid, quotadev, &fup->fu_usage);
		fup->fu_usage.du_curinodes = 0;
		fup->fu_usage.du_curblocks = 0;
	}
	ftruncate(fileno(qf), (highuid + 1) * sizeof(struct dqblk));
	return (0);
}

acct(ip)
	register struct dinode *ip;
{
	register n;
	register struct fileusage *fup;

	if (ip == NULL)
		return;
	if (ip->di_mode == 0)
		return;
	fup = lookup(ip->di_uid);
	if (fup == 0)
		fup = adduid(ip->di_uid);
	fup->fu_usage.du_curinodes++;
	if ((ip->di_mode & IFMT) == IFCHR || (ip->di_mode & IFMT) == IFBLK)
		return;
	fup->fu_usage.du_curblocks += ip->di_blocks;
}

oneof(target, list, n)
	char *target, *list[];
	register int n;
{
	register int i;

	for (i = 0; i < n; i++)
		if (strcmp(target, list[i]) == 0) {
			done |= 1 << i;
			return (1);
		}
	return (0);
}

struct dinode *
ginode()
{
	register unsigned long iblk;

	if (dp == NULL || ++dp >= &itab[ITABSZ]) {
		iblk = itod(&sblock, ino);
		bread(fsbtodb(&sblock, iblk), (char *)itab, sizeof itab);
		dp = &itab[ino % INOPB(&sblock)];
	}
	if (ino++ < ROOTINO)
		return(NULL);
	return(dp);
}

bread(bno, buf, cnt)
	long unsigned bno;
	char *buf;
{

	lseek(fi, (long)dbtob(bno), 0);
	if (read(fi, buf, cnt) != cnt) {
		printf("read error %u\n", bno);
		exit(1);
	}
}

struct fileusage *
lookup(uid)
	u_short uid;
{
	register struct fileusage *fup;

	for (fup = fuhead[uid % FUHASH]; fup != 0; fup = fup->fu_next)
		if (fup->fu_uid == uid)
			return (fup);
	return ((struct fileusage *)0);
}

struct fileusage *
adduid(uid)
	u_short uid;
{
	struct fileusage *fup, **fhp;

	fup = lookup(uid);
	if (fup != 0)
		return (fup);
	fup = (struct fileusage *)calloc(1, sizeof(struct fileusage));
	if (fup == 0) {
		fprintf(stderr, "out of memory for fileusage structures\n");
		exit(1);
	}
	fhp = &fuhead[uid % FUHASH];
	fup->fu_next = *fhp;
	*fhp = fup;
	fup->fu_uid = uid;
	if (uid > highuid)
		highuid = uid;
	return (fup);
}

char *
makerawname(name)
	char *name;
{
	register char *cp;
	char tmp, ch, *rindex();
	static char rawname[MAXPATHLEN];

	strcpy(rawname, name);
	cp = rindex(rawname, '/') + 1;
	if (cp == (char *)1 || *cp == 'r')
		return (name);
	for (ch = 'r'; *cp != '\0'; ) {
		tmp = *cp;
		*cp++ = ch;
		ch = tmp;
	}
	*cp++ = ch;
	*cp = '\0';
	return (rawname);
}
