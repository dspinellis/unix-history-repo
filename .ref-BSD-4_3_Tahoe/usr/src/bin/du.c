#ifndef lint
static char *sccsid = "@(#)du.c	4.12 (Berkeley) 10/22/87";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

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
char	*index(), *rindex(), *strcpy();

#define	kb(n)	(howmany(dbtob(n), 1024))

main(argc, argv)
	int argc;
	char **argv;
{
	long blocks = 0;
	register char *np;
	int pid;

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
		if (argc > 1) {
			pid = fork();
			if (pid == -1) {
				fprintf(stderr, "No more processes.\n");
				exit(1);
			}
			if (pid != 0)
				wait((int *)0);
		}
		if (argc == 1 || pid == 0) {
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
			blocks = descend(path, *np ? np : ".");
			if (sflg)
				printf("%ld\t%s\n", kb(blocks), path);
			if (argc > 1)
				exit(1);
		}
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
	long blocks = 0;
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
	blocks = stb.st_blocks;
	if ((stb.st_mode&S_IFMT) != S_IFDIR) {
		if (aflg)
			printf("%ld\t%s\n", kb(blocks), base);
		return (blocks);
	}
	if (dirp != NULL)
		closedir(dirp);
	dirp = opendir(name);
	if (dirp == NULL) {
		perror(base);
		*ebase0 = 0;
		return (0);
	}
	if (chdir(name) < 0) {
		perror(base);
		*ebase0 = 0;
		closedir(dirp);
		dirp = NULL;
		return (0);
	}
	while (dp = readdir(dirp)) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		(void) sprintf(ebase, "/%s", dp->d_name);
		curoff = telldir(dirp);
		blocks += descend(base, ebase+1);
		*ebase = 0;
		if (dirp == NULL) {
			dirp = opendir(".");
			if (dirp == NULL) {
				perror(".");
				return (0);
			}
			seekdir(dirp, curoff);
		}
	}
	closedir(dirp);
	dirp = NULL;
	if (sflg == 0)
		printf("%ld\t%s\n", kb(blocks), base);
	if (chdir("..") < 0) {
		(void) sprintf(index(base, 0), "/..");
		perror(base);
		exit(1);
	}
	*ebase0 = 0;
	return (blocks);
}
