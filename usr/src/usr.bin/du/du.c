#ifndef lint
static char *sccsid = "@(#)du.c	4.3 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ndir.h>

#define howmany(x, y)	((x + y - 1) / y)
#define EQ(x,y)	(strcmp(x,y)==0)
#define ML	1000

struct stat Statb;
char	path[256], name[256];
int	Aflag = 0,
	Sflag = 0,
	Noarg = 0;
struct {
	int	dev,
		ino;
} ml[ML];
long	descend();
char	*rindex();
char	*strcpy();

main(argc, argv)
char **argv;
{
	register	i = 1;
	long	blocks = 0;
	register char	*np;

	if (argc>1) {
		if(EQ(argv[i], "-s")) {
			++i;
			++Sflag;
		} else if(EQ(argv[i], "-a")) {
			++i;
			++Aflag;
		}
	}
	if(i == argc)
		++Noarg;

	do {
		(void) strcpy(path, Noarg? ".": argv[i]);
		(void) strcpy(name, path);
		if(np = rindex(name, '/')) {
			*np++ = '\0';
			if(chdir(*name? name: "/") == -1) {
				fprintf(stderr, "cannot chdir()\n");
				exit(1);
			}
		} else
			np = path;
		blocks = descend(path, *np? np: ".");
		if(Sflag)
			printf("%ld	%s\n", blocks, path);
	} while(++i < argc);

	exit(0);
}

DIR *dirp = NULL;
long
descend(np, fname)
char *np, *fname;
{
	register  struct direct *dp;
	register char *c1;
	int i;
	char *endofname;
	long blocks = 0;
	long curoff = NULL;

	if(stat(fname,&Statb)<0) {
		fprintf(stderr, "--bad status < %s >\n", name);
		return 0L;
	}
	if(Statb.st_nlink > 1 && (Statb.st_mode&S_IFMT)!=S_IFDIR) {
		static linked = 0;

		for(i = 0; i <= linked; ++i) {
			if(ml[i].ino==Statb.st_ino && ml[i].dev==Statb.st_dev)
				return 0;
		}
		if (linked < ML) {
			ml[linked].dev = Statb.st_dev;
			ml[linked].ino = Statb.st_ino;
			++linked;
		}
	}
	blocks = howmany(Statb.st_size, 1024);

	if((Statb.st_mode&S_IFMT) != S_IFDIR) {
		if(Aflag)
			printf("%ld	%s\n", blocks, np);
		return(blocks);
	}

	for(c1 = np; *c1; ++c1);
	if(*(c1-1) == '/')
		--c1;
	endofname = c1;
	if(chdir(fname) == -1)
		return 0;
	if (dirp != NULL)
		closedir(dirp);
	if ((dirp = opendir(".")) == NULL) {
		fprintf(stderr, "--cannot open < %s >\n", np);
		goto ret;
	}
	if ((dp = readdir(dirp)) == NULL) {
		fprintf(stderr, "--cannot read < %s >\n", np);
		closedir(dirp);
		dirp = NULL;
		goto ret;
	}
	for ( ; dp != NULL; dp = readdir(dirp)) {
		/* each directory entry */
		if (EQ(dp->d_name, ".") || EQ(dp->d_name, ".."))
			continue;
		c1 = endofname;
		*c1++ = '/';
		(void) strcpy(c1, dp->d_name);
		curoff = telldir(dirp);
		blocks += descend(np, endofname+1);
		if (dirp == NULL) {
			/* previous entry was a directory */
			dirp = opendir(".");
			seekdir(dirp, curoff);
		}
	}
	closedir(dirp);
	dirp = NULL;
	*endofname = '\0';
	if(!Sflag)
		printf("%ld	%s\n", blocks, np);
ret:
	if(chdir("..") == -1) {
		*endofname = '\0';
		fprintf(stderr, "Bad directory <%s>\n", np);
		while(*--endofname != '/');
		*endofname = '\0';
		if(chdir(np) == -1)
			exit(1);
	}
	return(blocks);
}
