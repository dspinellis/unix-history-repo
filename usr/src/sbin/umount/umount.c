static char *sccsid = "@(#)umount.c	4.4 (Berkeley) %G%";

#include <stdio.h>
#include <fstab.h>

/*
 * umount
 */

#define	NMOUNT	16
#define	NAMSIZ	32

struct mtab {
	char	file[NAMSIZ];
	char	spec[NAMSIZ];
} mtab[NMOUNT];

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
	if (!strcmp(*argv, "-v")) {
		vflag++;
		argc--, argv++;
		goto again;
	}
	if (!strcmp(*argv, "-a")) {
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
	struct fstab fs, *fsp;

	if ((fsp = getfsent()) == 0)
		return;
	fs = *fsp;	/* save info locally; it is static from getfsent() */
	umountall();
	if (strcmp(fs.fs_file, "/") == 0)
		return;
	if (strcmp(fs.fs_type, FSTAB_RW) && strcmp(fs.fs_type, FSTAB_RO))
		return;
	if (umountfs(fs.fs_spec) < 0) {
		perror(fs.fs_spec);
		return;
	}
}

struct	mtab zeromtab;

umountfs(name)
	char *name;
{
	register char *p1, *p2;
	register struct	mtab *mp;
	int mf;

	if (umount(name) < 0) {
		perror(name);
		return (0);
	}
	if (vflag)
		fprintf(stderr, "%s: Unmounted\n", name);
	while ((p1 = rindex(name, '/')) && p1[1] == 0)
		*p1 = 0;
	if (p1)
		name = p1 + 1;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++) {
		if (strncmp(mp->spec, name, sizeof (mp->spec)))
			continue;
		*mp = zeromtab;
		for (mp = &mtab[NMOUNT]; mp >= mtab; mp--)
			if (mp->file[0])
				break;
		mp++;
		mf = creat("/etc/mtab", 0644);
		write(mf, (char *)mtab, (mp-mtab) * sizeof (struct mtab));
		return (1);
	}
	fprintf(stderr, "%s: Not mounted\n", name);
	return (0);
}
