static char *sccsid = "@(#)mount.c	4.7 (Berkeley) %G%";
#include <stdio.h>
#include <fstab.h>

/*
 * mount
 */

#define	NMOUNT	16
#define	NAMSIZ	32

struct mtab {
	char	file[NAMSIZ];
	char	spec[NAMSIZ];
} mtab[NMOUNT];

int	all;
int	ro;
int	fake;
int	verbose;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mtab *mp;
	register char *np;
	int mf;

	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, NMOUNT*2*NAMSIZ);
	if (argc==1) {
		for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
			if (mp->file[0])
				printf("%s on %s\n", mp->spec, mp->file);
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
			ro++;
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
		while ( (fsp = getfsent()) != 0) {
			if (strcmp(fsp->fs_file, "/") == 0)
				continue;
			ro = !strcmp(fsp->fs_type, FSTAB_RO);
			if (ro==0 && strcmp(fsp->fs_type, FSTAB_RW))
				continue;
			mountfs(fsp->fs_spec, fsp->fs_file, ro);
		}
		exit(0);
	}
	if (argc < 2 || argc > 3) {
argcnt:
		fprintf(stderr,
	    "usage: mount [ -a ] [ -r ] [ -f ] [ -v ] [ special dir ]\n");
		exit(1);
	}
	mountfs(argv[1], argv[2], ro);
}

mountfs(spec, name, ro)
	char *spec, *name;
{
	register char *np;
	register struct mtab *mp;
	int mf;

	if (fake==0) {
		if (mount(spec, name, ro) < 0) {
			fprintf(stderr, "%s on ", spec);
			perror(name);
			return;
		}
	}
	if (verbose)
		fprintf(stderr, "%s on %s%s\n", spec, name,
		    ro ? " read only" : "");
	np = spec;
	while (*np++)
		;
	np--;
	while (*--np == '/')
		*np = '\0';
	while (np > spec && *--np != '/')
		;
	if (*np == '/')
		np++;
	spec = np;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (!strcmp(mp->spec, spec))
			goto replace;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
		if (mp->file[0] == 0)
			goto replace;
	return;
replace:
	for (np = mp->spec; np < &mp->spec[NAMSIZ-1];)
		if ((*np++ = *spec++) == 0)
			spec--;
	for (np = mp->file; np < &mp->file[NAMSIZ-1];)
		if ((*np++ = *name++) == 0)
			name--;
	mp = &mtab[NMOUNT];
	while ((--mp)->file[0] == 0);
	mf = creat("/etc/mtab", 0644);
	write(mf, (char *)mtab, (mp-mtab+1)*2*NAMSIZ);
	close(mf);
	return;
}
