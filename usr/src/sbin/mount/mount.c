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
static char sccsid[] = "@(#)mount.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * mount
 */
#include <sys/param.h>

#include <stdio.h>
#include <fstab.h>
#include <mtab.h>
#include <errno.h>

#define	DNMAX	(sizeof (mtab[0].m_dname) - 1)
#define	PNMAX	(sizeof (mtab[0].m_path) - 1)

struct	mtab mtab[NMOUNT];

int	all;
int	ro;
int	fake;
int	verbose;
char	*index(), *rindex();

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mtab *mp;
	register char *np;
	int mf;
	char *type = FSTAB_RW;

	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, sizeof (mtab));
	if (argc == 1) {
		for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
			if (mp->m_path[0] != '\0')
				prmtab(mp);
		exit(0);
	}
top:
	if (argc > 1) {
		if (!strcmp(argv[1], "-a")) {
			all++;
			argc--, argv++;
			goto top;
		}
		if (!strcmp(argv[1], "-r")) {
			type = FSTAB_RO;
			argc--, argv++;
			goto top;
		}
		if (!strcmp(argv[1], "-f")) {
			fake++;
			argc--, argv++;
			goto top;
		}
		if (!strcmp(argv[1], "-v")) {
			verbose++;
			argc--, argv++;
			goto top;
		}
	}
	if (all) {
		struct fstab *fsp;

		if (argc > 1)
			goto argcnt;
		close(2); dup(1);
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while ((fsp = getfsent()) != 0) {
			if (strcmp(fsp->fs_file, "/") == 0)
				continue;
			if (strcmp(fsp->fs_type, FSTAB_RO) &&
			    strcmp(fsp->fs_type, FSTAB_RW) &&
			    strcmp(fsp->fs_type, FSTAB_RQ))
				continue;
			mountfs(fsp->fs_spec, fsp->fs_file, fsp->fs_type);
		}
		exit(0);
	}
	if (argc == 2) {
		struct fstab *fs;

		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		fs = getfsfile(argv[1]);
		if (fs == NULL)
			goto argcnt;
		mountfs(fs->fs_spec, fs->fs_file, type);
		exit(0);
	}
	if (argc != 3) {
argcnt:
		fprintf(stderr,
    "usage: mount [ -a ] [ -r ] [ -f ] [ -v ] [ special dir ] [ dir ]\n");
		exit(1);
	}
	mountfs(argv[1], argv[2], type);
}

prmtab(mp)
	register struct mtab *mp;
{

	printf("%s on %s", mp->m_dname, mp->m_path);
	if (strcmp(mp->m_type, FSTAB_RO) == 0)
		printf("\t(read-only)");
	if (strcmp(mp->m_type, FSTAB_RQ) == 0)
		printf("\t(with quotas)");
	putchar('\n');
}

mountfs(spec, name, type)
	char *spec, *name, *type;
{
	register char *np;
	register struct mtab *mp;
	int mf;

	if (!fake) {
		if (mount(spec, name, strcmp(type, FSTAB_RO) == 0) < 0) {
			extern int errno;
			char *cp;

			fprintf(stderr, "%s on ", spec);
			switch (errno) {

			case EMFILE:
				cp = "Mount table full";
				break;

			case EINVAL:
				cp = "Bogus super block";
				break;

			default:
				perror(name);
				return;
			}
			fprintf(stderr, "%s: %s\n", name, cp);
			return;
		}
		/* we don't do quotas.... */
		if (strcmp(type, FSTAB_RQ) == 0)
			type = FSTAB_RW;
	}
	np = index(spec, '\0');
	while (*--np == '/')
		*np = '\0';
	np = rindex(spec, '/');
	if (np) {
		*np++ = '\0';
		spec = np;
	}
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (strcmp(mp->m_dname, spec) == 0)
			goto replace;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (mp->m_path[0] == '\0')
			goto replace;
	return;
replace:
	strncpy(mp->m_dname, spec, DNMAX);
	mp->m_dname[DNMAX] = '\0';
	strncpy(mp->m_path, name, PNMAX);
	mp->m_path[PNMAX] = '\0';
	strcpy(mp->m_type, type);
	if (verbose)
		prmtab(mp);
	mp = mtab + NMOUNT - 1;
	while (mp > mtab && mp->m_path[0] == '\0')
		--mp;
	mf = creat("/etc/mtab", 0644);
	write(mf, (char *)mtab, (mp - mtab + 1) * sizeof (struct mtab));
	close(mf);
	return;
}
