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
static char sccsid[] = "@(#)quotaon.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Turn quota on/off for a filesystem.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <stdio.h>
#include <fstab.h>
#include <mtab.h>

struct	mtab mtab[NMOUNT];

int	vflag;		/* verbose */
int	aflag;		/* all file systems */
int	done;
int	mf;

char	*qfname = "quotas";
char	quotafile[MAXPATHLEN + 1];
char	*index(), *rindex();

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	char *whoami, *rindex();
	int offmode = 0, errs = 0, i;

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
		fprintf(stderr, "Usage:\n\t%s [-v] -a\n\t%s [-v] filesys ...\n",
			whoami, whoami);
		exit(1);
	}
	mf = open("/etc/mtab", O_RDONLY);
	if (mf < 0) {
		perror("/etc/mtab");
		exit(1);
	}
	(void) read(mf, (char *)mtab, sizeof (mtab));
	close(mf);
	setfsent();
	while ((fs = getfsent()) != NULL) {
		if (aflag &&
		    (fs->fs_type == 0 || strcmp(fs->fs_type, "rq") != 0))
			continue;
		if (!aflag &&
		    !(oneof(fs->fs_file, argv, argc) ||
		      oneof(fs->fs_spec, argv, argc)))
			continue;
		errs += quotaonoff(fs, offmode);
	}
	endfsent();
	for (i = 0; i < argc; i++)
		if ((done & (1 << i)) == 0)
			fprintf(stderr, "%s not found in /etc/fstab\n",
				argv[i]);
	exit(errs);
}

quotaonoff(fs, offmode)
	register struct fstab *fs;
	int offmode;
{

	if (strcmp(fs->fs_file, "/") && readonly(fs))
		return (1);
	if (offmode) {
		if (setquota(fs->fs_spec, NULL) < 0)
			goto bad;
		if (vflag)
			printf("%s: quotas turned off\n", fs->fs_file);
		changemtab(fs, FSTAB_RW);
		return (0);
	}
	(void) sprintf(quotafile, "%s/%s", fs->fs_file, qfname);
	if (setquota(fs->fs_spec, quotafile) < 0)
		goto bad;
	if (vflag)
		printf("%s: quotas turned on\n", fs->fs_file);
	changemtab(fs, FSTAB_RQ);
	return (0);
bad:
	fprintf(stderr, "setquota: ");
	perror(fs->fs_spec);
	return (1);
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

changemtab(fs, type)
	register struct fstab *fs;
	char *type;
{
	register struct mtab *mp;
	register char *cp;

	cp = index(fs->fs_spec, '\0');
	while (*--cp == '/')
		*cp = '\0';
	cp = rindex(fs->fs_spec, '/');
	if (cp)
		cp++;
	else
		cp = fs->fs_spec;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (strcmp(mp->m_dname, cp) == 0)
			goto replace;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (mp->m_path[0] == '\0')
			goto replace;
	return;
replace:
	strcpy(mp->m_type, type);
	mp = mtab + NMOUNT - 1;
	while (mp > mtab && mp->m_path[0] == '\0')
		--mp;
	mf = creat("/etc/mtab", 0644);
	(void) write(mf, (char *)mtab, (mp - mtab + 1) * sizeof (struct mtab));
	close(mf);
}

/*
 * Verify file system is mounted and not readonly.
 */
readonly(fs)
	register struct fstab *fs;
{
	register struct mtab *mp;
	register char *cp;

	cp = index(fs->fs_spec, '\0');
	while (*--cp == '/')
		*cp = '\0';
	cp = rindex(fs->fs_spec, '/');
	if (cp)
		cp++;
	else
		cp = fs->fs_spec;
	for (mp = mtab; mp < mtab + NMOUNT; mp++) {
		if (mp->m_path[0] == '\0')
			break;
		if (strcmp(cp, mp->m_dname) == 0) {
			if (strcmp(mp->m_type, FSTAB_RO) == 0) {
				printf("%s: mounted read-only\n", fs->fs_file);
				return (1);
			}
			return (0);
		}
	}
	printf("%s: not mounted\n", fs->fs_file);
	return (1);
}
