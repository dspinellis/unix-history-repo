/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)quotacheck.c	5.10 (Berkeley) %G%";
#endif /* not lint */

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
#include <sys/wait.h>
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
int	pflag;		/* fsck like parallel check */

char *qfname = "quotas";
char quotafile[MAXPATHLEN + 1];
struct dqblk zerodqbuf;
struct fileusage zerofileusage;
long dev_bsize = 1;

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
	if (argc > 0 && strcmp(*argv, "-p") == 0) {
		pflag++;
		goto again;
	}
	if (argc <= 0 && !aflag) {
		fprintf(stderr, "Usage:\n\t%s\n\t%s\n",
			"quotacheck [-v] [-p] -a",
			"quotacheck [-v] [-p] filesys ...");
		exit(1);
	}

	setpwent();
	while ((pw = getpwent()) != 0) {
		fup = lookup(pw->pw_uid);
		if (fup == 0) {
			fup = adduid(pw->pw_uid);
			strncpy(fup->fu_name, pw->pw_name,
				sizeof(fup->fu_name));
		}
	}
	endpwent();

	if (pflag)
		errs = preen(argc, argv);
	else {
		if (setfsent() == 0) {
			fprintf(stderr, "Can't open ");
			perror(FSTAB);
			exit(8);
		}
		while ((fs = getfsent()) != NULL) {
			if (aflag &&
			    (fs->fs_type == 0 ||
			     strcmp(fs->fs_type, FSTAB_RQ) != 0))
				continue;
			if (!aflag &&
			    !(oneof(fs->fs_file, argv, argc) ||
			      oneof(fs->fs_spec, argv, argc)))
				continue;
			(void) sprintf(quotafile, "%s/%s", fs->fs_file, qfname);
			errs += chkquota(fs->fs_spec, fs->fs_file, quotafile);
		}
		endfsent();
	}

	for (i = 0; i < argc; i++)
		if ((done & (1 << i)) == 0)
			fprintf(stderr, "%s not found in %s\n",
				argv[i], FSTAB);
	exit(errs);
}

preen(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	register int passno, anygtr;
	register int errs;
	union wait status;

	passno = 1;
	errs = 0;
	do {
		anygtr = 0;

		if (setfsent() == 0) {
			fprintf(stderr, "Can't open ");
			perror(FSTAB);
			exit(8);
		}

		while ((fs = getfsent()) != NULL) {
			if (fs->fs_passno > passno)
				anygtr = 1;

			if (aflag &&
			    (fs->fs_type == 0 ||
			     strcmp(fs->fs_type, FSTAB_RQ) != 0))
				continue;

			if (!aflag &&
			    !(oneof(fs->fs_file, argv, argc) ||
			      oneof(fs->fs_spec, argv, argc)))
				continue;

			if (fs->fs_passno != passno)
				continue;

			switch (fork()) {
			case -1:
				perror("fork");
				exit(8);
				break;

			case 0:
				(void) sprintf(quotafile, "%s/%s",
					fs->fs_file, qfname);
				exit(chkquota(fs->fs_spec,
					fs->fs_file, quotafile));
			}
		}

		while (wait(&status) != -1) 
			errs += status.w_retcode;

		passno++;
	} while (anygtr);

	return (errs);
}

chkquota(fsdev, fsfile, qffile)
	char *fsdev;
	char *fsfile;
	char *qffile;
{
	register struct fileusage *fup;
	dev_t quotadev;
	register FILE *qfi, *qfo;
	u_short uid;
	int cg, i, fdo;
	char *rawdisk;
	struct stat statb;
	struct dqblk dqbuf;
	static int warned = 0;
	extern int errno;

	rawdisk = makerawname(fsdev);
	if (vflag)
		fprintf(stdout, "*** Checking quotas for %s (%s)\n", rawdisk, fsfile);
	fi = open(rawdisk, 0);
	if (fi < 0) {
		perror(rawdisk);
		return (1);
	}
	qfi = fopen(qffile, "r");
	if (qfi == NULL) {
		perror(qffile);
		close(fi);
		return (1);
	}
	if (fstat(fileno(qfi), &statb) < 0) {
		perror(qffile);
		fclose(qfi);
		close(fi);
		return (1);
	}
	quotadev = statb.st_dev;
	if (stat(fsdev, &statb) < 0) {
		perror(fsdev);
		fclose(qfi);
		close(fi);
		return (1);
	}
	if (quotadev != statb.st_rdev) {
		fprintf(stderr, "%s dev (0x%x) mismatch %s dev (0x%x)\n",
			qffile, quotadev, fsdev, statb.st_rdev);
		fclose(qfi);
		close(fi);
		return (1);
	}
	/*
	 * Must do fdopen(open(qffile, 1), "w") instead of fopen(qffile, "w")
	 * because fopen(qffile, "w") would truncate the quota file.
	 */
	fdo = open(qffile, 1);
	if (fdo < 0 || (qfo = fdopen(fdo, "w")) == NULL) {
		perror(qffile);
		if (fdo >= 0)
			close(fdo);
		fclose(qfi);
		close(fi);
		return (1);
	}
	if (quota(Q_SYNC, 0, quotadev, (caddr_t)0) < 0 &&
	    errno == EINVAL && !warned && vflag) {
		warned++;
		fprintf(stdout,
		    "*** Warning: Quotas are not compiled into this kernel\n");
	}
	sync();
	bread(SBOFF, (char *)&sblock, SBSIZE);
	dev_bsize = sblock.fs_fsize / fsbtodb(&sblock, 1);
	ino = 0;
	for (cg = 0; cg < sblock.fs_ncg; cg++) {
		dp = NULL;
		for (i = 0; i < sblock.fs_ipg; i++)
			acct(ginode());
	}
	for (uid = 0; uid <= highuid; uid++) {
		i = fread(&dqbuf, sizeof(struct dqblk), 1, qfi);
		if (i == 0)
			dqbuf = zerodqbuf;
		fup = lookup(uid);
		if (fup == 0)
			fup = &zerofileusage;
		if (dqbuf.dqb_curinodes == fup->fu_usage.du_curinodes &&
		    dqbuf.dqb_curblocks == fup->fu_usage.du_curblocks) {
			fup->fu_usage.du_curinodes = 0;
			fup->fu_usage.du_curblocks = 0;
			fseek(qfo, (long)sizeof(struct dqblk), 1);
			continue;
		}
		if (vflag) {
			if (pflag)
				printf("%s: ", rawdisk);
			if (fup->fu_name[0] != '\0')
				printf("%-8s fixed:", fup->fu_name);
			else
				printf("#%-7d fixed:", uid);
			if (dqbuf.dqb_curinodes != fup->fu_usage.du_curinodes)
				fprintf(stdout, "\tinodes %d -> %d",
					dqbuf.dqb_curinodes, fup->fu_usage.du_curinodes);
			if (dqbuf.dqb_curblocks != fup->fu_usage.du_curblocks)
				fprintf(stdout, "\tblocks %d -> %d",
					dqbuf.dqb_curblocks, fup->fu_usage.du_curblocks);
			fprintf(stdout, "\n");
		}
		dqbuf.dqb_curinodes = fup->fu_usage.du_curinodes;
		dqbuf.dqb_curblocks = fup->fu_usage.du_curblocks;
		fwrite(&dqbuf, sizeof(struct dqblk), 1, qfo);
		quota(Q_SETDUSE, uid, quotadev, &fup->fu_usage);
		fup->fu_usage.du_curinodes = 0;
		fup->fu_usage.du_curblocks = 0;
	}
	fflush(qfo);
	ftruncate(fileno(qfo), (off_t)((highuid + 1) * sizeof(struct dqblk)));
	fclose(qfi);
	fclose(qfo);
	close(fi);
	return (0);
}

acct(ip)
	register struct dinode *ip;
{
	register struct fileusage *fup;

	if (ip == NULL)
		return;
	if (ip->di_mode == 0)
		return;
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

	if (lseek(fi, bno * dev_bsize, 0) < 0) {
		perror("lseek");
		exit(1);
	}

	if (read(fi, buf, cnt) != cnt) {
		perror("read");
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
	extern char *calloc();

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
	cp = rindex(rawname, '/');
	if (cp == NULL)
		return (name);
	else
		cp++;
	for (ch = 'r'; *cp != '\0'; ) {
		tmp = *cp;
		*cp++ = ch;
		ch = tmp;
	}
	*cp++ = ch;
	*cp = '\0';
	return (rawname);
}
