/*
 * Copyright (c) 1980, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Robert Elz at The University of Melbourne.
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
"@(#) Copyright (c) 1980, 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)quotaon.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 * Turn quota on/off for a filesystem.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <ufs/quota.h>
#include <stdio.h>
#include <fstab.h>

int	aflag;		/* all file systems */
int	gflag;		/* operate on group quotas */
int	uflag;		/* operate on user quotas */
int	vflag;		/* verbose */

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	char ch, *whoami, *rindex();
	long argnum, done = 0;
	int i, offmode = 0, errs = 0;
	extern char *optarg;
	extern int optind;

	whoami = rindex(*argv, '/') + 1;
	if (whoami == (char *)1)
		whoami = *argv;
	if (strcmp(whoami, "quotaoff") == 0)
		offmode++;
	else if (strcmp(whoami, "quotaon") != 0) {
		fprintf(stderr, "Name must be quotaon or quotaoff not %s\n",
			whoami);
		exit(1);
	}
	while ((ch = getopt(argc, argv, "avug")) != EOF) {
		switch(ch) {
		case 'a':
			aflag++;
			break;
		case 'g':
			gflag++;
			break;
		case 'u':
			uflag++;
			break;
		case 'v':
			vflag++;
			break;
		default:
			usage(whoami);
		}
	}
	argc -= optind;
	argv += optind;
	if (argc <= 0 && !aflag)
		usage(whoami);
	if (!gflag && !uflag) {
		gflag++;
		uflag++;
	}
	setfsent();
	while ((fs = getfsent()) != NULL) {
		if (aflag) {
			if (gflag && hasquota(fs->fs_mntops, GRPQUOTA))
				errs += quotaonoff(fs, offmode, GRPQUOTA);
			if (uflag && hasquota(fs->fs_mntops, USRQUOTA))
				errs += quotaonoff(fs, offmode, USRQUOTA);
			continue;
		}
		if ((argnum = oneof(fs->fs_file, argv, argc) >= 0) ||
		    (argnum = oneof(fs->fs_spec, argv, argc)) >= 0) {
			done |= 1 << argnum;
			if (gflag && hasquota(fs->fs_mntops, GRPQUOTA))
				errs += quotaonoff(fs, offmode, GRPQUOTA);
			if (uflag && hasquota(fs->fs_mntops, USRQUOTA))
				errs += quotaonoff(fs, offmode, USRQUOTA);
		}
	}
	endfsent();
	for (i = 0; i < argc; i++)
		if ((done & (1 << i)) == 0)
			fprintf(stderr, "%s not found in fstab\n",
				argv[i]);
	exit(errs);
}

usage(whoami)
	char *whoami;
{

	fprintf(stderr, "Usage:\n\t%s [-g] [-u] [-v] -a\n", whoami);
	fprintf(stderr, "\t%s [-g] [-u] [-v] filesys ...\n", whoami);
	exit(1);
}

quotaonoff(fs, offmode, type)
	register struct fstab *fs;
	int offmode, type;
{
	char	quotafile[MAXPATHLEN + 1];

	if (strcmp(fs->fs_file, "/") && readonly(fs))
		return (1);
	if (offmode) {
		if (quotactl(fs->fs_file, QCMD(Q_QUOTAOFF, type), 0, 0) < 0)
			goto bad;
		if (vflag)
			printf("%s: quotas turned off\n", fs->fs_file);
		return (0);
	}
	(void) sprintf(quotafile, "%s/%s.%s", fs->fs_file, qfname,
	    qfextension[type]);
	if (quotactl(fs->fs_file, QCMD(Q_QUOTAON, type), 0, quotafile) < 0)
		goto bad;
	if (vflag)
		printf("%s: %s quotas turned on\n", fs->fs_file,
		    qfextension[type]);
	return (0);
bad:
	fprintf(stderr, "setquota: ");
	perror(fs->fs_file);
	return (1);
}

/*
 * Check to see if target appears in list of size cnt.
 */
oneof(target, list, cnt)
	register char *target, *list[];
	int cnt;
{
	register int i;

	for (i = 0; i < cnt; i++)
		if (strcmp(target, list[i]) == 0)
			return (i);
	return (-1);
}

/*
 * Check to see if a particular quota is to be enabled.
 */
hasquota(options, type)
	char *options;
	int type;
{
	register char *opt;
	char buf[BUFSIZ];
	char *strtok();
	static char initname, usrname[100], grpname[100];

	if (!initname) {
		sprintf(usrname, "%s%s", qfextension[USRQUOTA], qfname);
		sprintf(grpname, "%s%s", qfextension[GRPQUOTA], qfname);
		initname = 1;
	}
	strcpy(buf, options);
	for (opt = strtok(buf, ","); opt; opt = strtok(NULL, ",")) {
		if (type == USRQUOTA && strcmp(opt, usrname) == 0)
			return(1);
		if (type == GRPQUOTA && strcmp(opt, grpname) == 0)
			return(1);
	}
	return (0);
}

/*
 * Verify file system is mounted and not readonly.
 */
readonly(fs)
	register struct fstab *fs;
{
	struct statfs fsbuf;

	if (statfs(fs->fs_file, &fsbuf) < 0 ||
	    strcmp(fsbuf.f_mntonname, fs->fs_file) ||
	    strcmp(fsbuf.f_mntfromname, fs->fs_spec)) {
		printf("%s: not mounted\n", fs->fs_file);
		return (1);
	}
	if (fsbuf.f_flags & MNT_RDONLY) {
		printf("%s: mounted read-only\n", fs->fs_file);
		return (1);
	}
	return (0);
}
