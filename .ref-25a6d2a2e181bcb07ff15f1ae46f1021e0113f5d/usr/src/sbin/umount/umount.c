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
static char sccsid[] = "@(#)umount.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * umount
 */
#include <sys/param.h>

#include <stdio.h>
#include <fstab.h>
#include <mtab.h>
#include </usr/src/local/mkmsys/macklem/mkmsys/h/mount.h>

struct	mtab mtab[NMOUNT];

char	*rindex();
int	vflag, all, errs;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mtab *mp;
	register char *p1, *p2;
	int mf;

	argc--, argv++;
	sync();
	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, sizeof (mtab));
again:
	if (argc > 0 && !strcmp(*argv, "-v")) {
		vflag++;
		argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-a")) {
		all++;
		argc--, argv++;
		goto again;
	}
	if (argc == 0 && !all) {
		fprintf(stderr, "Usage: umount [ -a ] [ -v ] [ dev ... ]\n");
		exit(1);
	}
	if (all) {
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		umountall();
		exit(0);
	}
	while (argc > 0) {
		if (umountfs(*argv++) == 0)
			errs++;
		argc--;
	}
	exit(errs);
}

umountall()
{
	struct fstab *fs, *allocfsent();

	if ((fs = getfsent()) == 0)
		return;
	fs = allocfsent(fs);
	umountall();
	if (strcmp(fs->fs_file, "/") == 0) {
		freefsent(fs);
		return;
	}
	if (strcmp(fs->fs_type, FSTAB_RW) &&
	    strcmp(fs->fs_type, FSTAB_RO) &&
	    strcmp(fs->fs_type, FSTAB_RQ)) {
		freefsent(fs);
		return;
	}
	if (umountfs(fs->fs_file) < 0)
		perror(fs->fs_file);
	freefsent(fs);
}

struct fstab *
allocfsent(fs)
	register struct fstab *fs;
{
	register struct fstab *new;
	register char *cp;
	char *malloc();

	new = (struct fstab *)malloc(sizeof (*fs));
	cp = malloc(strlen(fs->fs_file) + 1);
	strcpy(cp, fs->fs_file);
	new->fs_file = cp;
	cp = malloc(strlen(fs->fs_type) + 1);
	strcpy(cp, fs->fs_type);
	new->fs_type = cp;
	cp = malloc(strlen(fs->fs_spec) + 1);
	strcpy(cp, fs->fs_spec);
	new->fs_spec = cp;
	new->fs_passno = fs->fs_passno;
	new->fs_freq = fs->fs_freq;
	return (new);
}

freefsent(fs)
	register struct fstab *fs;
{

	if (fs->fs_file)
		free(fs->fs_file);
	if (fs->fs_spec)
		free(fs->fs_spec);
	if (fs->fs_type)
		free(fs->fs_type);
	free((char *)fs);
}

struct	mtab zeromtab;

umountfs(name)
	char *name;
{
	register char *p1, *p2;
	register struct	mtab *mp;
	int mf;
	struct fstab *fs;

	fs = getfsspec(name);
	if (fs != NULL)
		name = fs->fs_file;
	if (umount(name, MNT_NOFORCE) < 0) {
		perror(name);
		return (0);
	}
	if (vflag)
		fprintf(stderr, "%s: Unmounted\n", name);
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++) {
		if (strncmp(mp->m_path, name, sizeof (mp->m_path)))
			continue;
		*mp = zeromtab;
		for (mp = &mtab[NMOUNT]; mp >= mtab; mp--)
			if (mp->m_path[0])
				break;
		mp++;
		mf = creat("/etc/mtab", 0644);
		write(mf, (char *)mtab, (mp-mtab) * sizeof (struct mtab));
		return (1);
	}
	fprintf(stderr, "%s: Not mounted\n", name);
	return (0);
}
