#ifndef lint
static char *sccsid = "@(#)du.c	4.4 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ndir.h>

#define howmany(x, y)	(((x) + (y) - 1) / (y))

char	path[BUFSIZ], name[BUFSIZ];
int	aflg;
int	sflg;
char	*dot = ".";

#define ML	1000
struct {
	int	dev;
	ino_t	ino;
} ml[ML];
int	mlx;

long	descend();
char	*index(), *rindex(), *strcpy(), *sprintf();

main(argc, argv)
	int argc;
	char **argv;
{
	long kbytes = 0;
	register char *np;

	argc--, argv++;
again:
	if (argc && !strcmp(*argv, "-s")) {
		sflg++;
		argc--, argv++;
		goto again;
	}
	if (argc && !strcmp(*argv, "-a")) {
		aflg++;
		argc--, argv++;
		goto again;
	}
	if (argc == 0) {
		argv = &dot;
		argc = 1;
	}
	do {
		(void) strcpy(path, *argv);
		(void) strcpy(name, *argv);
		if (np = rindex(name, '/')) {
			*np++ = '\0';
			if (chdir(*name ? name : "/") < 0) {
				perror(*name ? name : "/");
				exit(1);
			}
		} else
			np = path;
		kbytes = descend(path, *np ? np : ".");
		if (sflg)
			printf("%ld\t%s\n", kbytes, path);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

DIR	*dirp = NULL;

long
descend(base, name)
	char *base, *name;
{
	char *ebase0, *ebase;
	struct stat stb;
	int i;
	long kbytes = 0;
	long curoff = NULL;
	register struct direct *dp;

	ebase0 = ebase = index(base, 0);
	if (ebase > base && ebase[-1] == '/')
		ebase--;
	if (lstat(name, &stb) < 0) {
		perror(base);
		*ebase0 = 0;
		return (0);
	}
	if (stb.st_nlink > 1 && (stb.st_mode&S_IFMT) != S_IFDIR) {
		for (i = 0; i <= mlx; i++)
			if (ml[i].ino == stb.st_ino && ml[i].dev == stb.st_dev)
				return (0);
		if (mlx < ML) {
			ml[mlx].dev = stb.st_dev;
			ml[mlx].ino = stb.st_ino;
			mlx++;
		}
	}
	kbytes = howmany(stb.st_size, 1024);
	if ((stb.st_mode&S_IFMT) != S_IFDIR) {
		if (aflg)
			printf("%ld\t%s\n", kbytes, base);
		return (kbytes);
	}
	if (chdir(name) < 0)
		return (0);
	if (dirp != NULL)
		closedir(dirp);
	dirp = opendir(".");
	if (dirp == NULL) {
		perror(base);
		*ebase0 = 0;
		return (0);
	}
	while (dp = readdir(dirp)) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		(void) sprintf(ebase, "/%s", dp->d_name);
		curoff = telldir(dirp);
		kbytes += descend(base, ebase+1);
		*ebase = 0;
		if (dirp == NULL) {
			dirp = opendir(".");
			seekdir(dirp, curoff);
		}
	}
	closedir(dirp);
	dirp = NULL;
	if (sflg == 0)
		printf("%ld\t%s\n", kbytes, base);
	if (chdir("..") < 0) {
		(void) sprintf(index(base, 0), "/..");
		perror(base);
		exit(1);
	}
	*ebase0 = 0;
	return (kbytes);
}
