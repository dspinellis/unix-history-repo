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
static char sccsid[] = "@(#)repquota.c	5.6 (Berkeley) %G%";
#endif /* not lint */

/*
 * Quota report
 */
#include <stdio.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/quota.h>
#include <sys/stat.h>
#include <fstab.h>
#include <pwd.h>

#define LOGINNAMESIZE 8
struct fileusage {
	struct fileusage *fu_next;
	struct dqblk fu_dqblk;
	u_short	fu_uid;
	char fu_name[LOGINNAMESIZE + 1];
};
#define FUHASH 997
struct fileusage *fuhead[FUHASH];
struct fileusage *lookup();
struct fileusage *adduid();
int highuid;

long done;
struct	passwd	*getpwent();

int	vflag;		/* verbose */
int	aflag;		/* all file systems */

char *qfname = "quotas";
struct dqblk zerodqblk;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	register struct passwd *pw;
	register struct fileusage *fup;
	char quotafile[MAXPATHLEN];
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
			"repquota [-v] -a",
			"repquota [-v] filesys ...");
		exit(1);
	}
	setpwent();
	while ((pw = getpwent()) != 0) {
		fup = lookup(pw->pw_uid);
		if (fup == 0) {
			fup = adduid(pw->pw_uid);
			strncpy(fup->fu_name, pw->pw_name, sizeof(fup->fu_name));
		}
	}
	endpwent();
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
		errs += repquota(fs->fs_spec, fs->fs_file, quotafile);
	}
	endfsent();
	for (i = 0; i < argc; i++)
		if ((done & (1 << i)) == 0)
			fprintf(stderr, "%s not found in /etc/fstab\n",
				argv[i]);
	exit(errs);
}

repquota(fsdev, fsfile, qffile)
	char *fsdev;
	char *fsfile;
	char *qffile;
{
	register struct fileusage *fup;
	FILE *qf;
	u_short uid;
	struct dqblk dqbuf;
	struct stat statb;
	static int warned = 0;
	extern int errno;

	if (vflag)
		fprintf(stdout, "*** Quota report for %s (%s)\n", fsdev, fsfile);
	qf = fopen(qffile, "r");
	if (qf == NULL) {
		perror(qffile);
		return (1);
	}
	if (fstat(fileno(qf), &statb) < 0) {
		perror(qffile);
		fclose(qf);
		return (1);
	}
	if (quota(Q_SYNC, 0, statb.st_dev, 0) < 0 &&
	    errno == EINVAL && !warned && vflag) {
		warned++;
		fprintf(stdout,
		    "*** Warning: Quotas are not compiled into this kernel\n");
	}
	for (uid = 0; ; uid++) {
		fread(&dqbuf, sizeof(struct dqblk), 1, qf);
		if (feof(qf))
			break;
		fup = lookup(uid);
		if ((fup == 0 || fup->fu_name[0] == 0) &&
		    dqbuf.dqb_curinodes == 0 && dqbuf.dqb_curblocks == 0)
			continue;
		if (fup == 0)
			fup = adduid(uid);
		fup->fu_dqblk = dqbuf;
	}
	printf("                        Block limits               File limits\n");
	printf("User            used    soft    hard  warn    used  soft  hard  warn\n");
	for (uid = 0; uid <= highuid; uid++) {
		fup = lookup(uid);
		if (fup == 0)
			continue;
		if (fup->fu_dqblk.dqb_curinodes == 0 &&
		    fup->fu_dqblk.dqb_curblocks == 0)
			continue;
		if (fup->fu_name[0] != '\0')
			printf("%-10s", fup->fu_name);
		else
			printf("#%-9d", uid);
		printf("%c%c%8d%8d%8d %5d   %5d %5d %5d %5d\n",
			fup->fu_dqblk.dqb_bsoftlimit && 
			    fup->fu_dqblk.dqb_curblocks >= 
			    fup->fu_dqblk.dqb_bsoftlimit ? '+' : '-',
			fup->fu_dqblk.dqb_isoftlimit &&
			    fup->fu_dqblk.dqb_curinodes >=
			    fup->fu_dqblk.dqb_isoftlimit ? '+' : '-',
			dbtob(fup->fu_dqblk.dqb_curblocks) / 1024,
			dbtob(fup->fu_dqblk.dqb_bsoftlimit) / 1024,
			dbtob(fup->fu_dqblk.dqb_bhardlimit) / 1024,
			fup->fu_dqblk.dqb_bwarn,
			fup->fu_dqblk.dqb_curinodes,
			fup->fu_dqblk.dqb_isoftlimit,
			fup->fu_dqblk.dqb_ihardlimit,
			fup->fu_dqblk.dqb_iwarn);
		fup->fu_dqblk = zerodqblk;
	}
	fclose(qf);
	return (0);
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
