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
static char sccsid[] = "@(#)mount.c	5.6 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <fstab.h>
#include <mtab.h>
#include <errno.h>
#include <stdio.h>

#define	BADTYPE(type) \
	(strcmp(type, FSTAB_RO) && strcmp(type, FSTAB_RW) && \
	    strcmp(type, FSTAB_RQ))
#define	SETTYPE(type) \
	(!strcmp(type, FSTAB_RW) || !strcmp(type, FSTAB_RQ))

#define	MTAB	"/etc/mtab"

static struct mtab mtab[NMOUNT];
static int fake, verbose;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct mtab *mp;
	register struct fstab *fs;
	register int cnt;
	int all, ch, fd, rval, sfake;
	char *type;

	all = 0;
	type = NULL;
	while ((ch = getopt(argc, argv, "afrwv")) != EOF)
		switch((char)ch) {
		case 'a':
			all = 1;
			break;
		case 'f':
			fake = 1;
			break;
		case 'r':
			type = FSTAB_RO;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'w':
			type = FSTAB_RW;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* NOSTRICT */
	if ((fd = open(MTAB, O_RDONLY, 0)) >= 0) {
		if (read(fd, (char *)mtab, NMOUNT * sizeof(struct mtab)) < 0)
			mtaberr();
		(void)close(fd);
	}

	if (all) {
		rval = 0;
		for (sfake = fake; fs = getfsent(); fake = sfake) {
			if (BADTYPE(fs->fs_type))
				continue;
			/* `/' is special, it's always mounted */
			if (!strcmp(fs->fs_file, "/"))
				fake = 1;
			rval |= mountfs(fs->fs_spec, fs->fs_file,
			    type ? type : fs->fs_type);
		}
		exit(rval);
	}

	if (argc == 0) {
		if (verbose || fake || type)
			usage();
		for (mp = mtab, cnt = NMOUNT; cnt--; ++mp)
			if (*mp->m_path)
				prmtab(mp);
		exit(0);
	}

	if (all)
		usage();

	if (argc == 1) {
		if (!(fs = getfsfile(*argv)) && !(fs = getfsspec(*argv))) {
			fprintf(stderr,
			    "mount: unknown special file or file system %s.\n",
			    *argv);
			exit(1);
		}
		if (BADTYPE(fs->fs_type)) {
			fprintf(stderr,
			    "mount: %s has unknown file system type.\n", *argv);
			exit(1);
		}
		exit(mountfs(fs->fs_spec, fs->fs_file,
		    type ? type : fs->fs_type));
	}

	if (argc != 2)
		usage();

	exit(mountfs(argv[0], argv[1], type ? type : "rw"));
}

static
mountfs(spec, name, type)
	char *spec, *name, *type;
{
	extern int errno;
	register struct mtab *mp, *space;
	register int cnt;
	register char *p;
	int fd;
	char *index(), *rindex(), *strcpy();

	if (!fake) {
		if (mount(spec, name, !strcmp(type, FSTAB_RO))) {
			fprintf(stderr, "%s on %s: ", spec, name);
			switch (errno) {
			case EMFILE:
				fprintf(stderr, "Mount table full\n");
				break;
			case EINVAL:
				fprintf(stderr, "Bogus super block\n");
				break;
			default:
				perror((char *)NULL);
				break;
			}
			return(1);
		}

		/* we don't do quotas.... */
		if (strcmp(type, FSTAB_RQ) == 0)
			type = FSTAB_RW;
	}

	/* trim trailing /'s and find last component of name */
	for (p = index(spec, '\0'); *--p == '/';);
	*++p = '\0';
	if (p = rindex(spec, '/')) {
		*p = '\0';
		spec = p + 1;
	}

	for (mp = mtab, cnt = NMOUNT, space = NULL; cnt--; ++mp) {
		if (!strcmp(mp->m_dname, spec))
			break;
		if (!space && !*mp->m_path)
			space = mp;
	}
	if (cnt == -1) {
		if (!space) {
			fprintf(stderr, "mount: no more room in %s.\n", MTAB);
			exit(1);
		}
		mp = space;
	}

#define	DNMAX	(sizeof(mtab[0].m_dname) - 1)
#define	PNMAX	(sizeof(mtab[0].m_path) - 1)

	(void)strncpy(mp->m_dname, spec, DNMAX);
	mp->m_dname[DNMAX] = '\0';
	(void)strncpy(mp->m_path, name, PNMAX);
	mp->m_path[PNMAX] = '\0';
	(void)strcpy(mp->m_type, type);

	if (verbose)
		prmtab(mp);

	for (mp = mtab + NMOUNT - 1; mp > mtab && !*mp->m_path; --mp);
	if ((fd = open(MTAB, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0)
		mtaberr();
	cnt = (mp - mtab + 1) * sizeof(struct mtab);
	/* NOSTRICT */
	if (write(fd, (char *)mtab, cnt) != cnt)
		mtaberr();
	(void)close(fd);
	return(0);
}

static
prmtab(mp)
	register struct mtab *mp;
{
	printf("%s on %s", mp->m_dname, mp->m_path);
	if (!strcmp(mp->m_type, FSTAB_RO))
		printf("\t(read-only)");
	else if (!strcmp(mp->m_type, FSTAB_RQ))
		printf("\t(with quotas)");
	printf("\n");
}

static
mtaberr()
{
	fprintf(stderr, "mount: %s: ", MTAB);
	perror((char *)NULL);
	exit(1);
}

static
usage()
{
	fprintf(stderr, "usage: mount [-afrw]\nor mount [-frw] special | node\nor mount [-frw] special node\n");
	exit(1);
}
