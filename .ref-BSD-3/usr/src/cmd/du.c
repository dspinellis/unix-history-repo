#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
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
		strcpy(path, Noarg? ".": argv[i]);
		strcpy(name, path);
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

long
descend(np, fname)
char *np, *fname;
{
	int dir = 0, /* open directory */
		offset,
		dsize,
		entries,
		dirsize;

	struct direct dentry[BUFSIZ / sizeof (struct direct)];
	register  struct direct *dp;
	register char *c1, *c2;
	int i;
	char *endofname;
	long blocks = 0;

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
/*
	blocks = (Statb.st_size + BSIZE-1) >> BSHIFT;
*/
	blocks = (Statb.st_size + 511) >> 9;

	if((Statb.st_mode&S_IFMT)!=S_IFDIR) {
		if(Aflag)
			printf("%ld	%s\n", blocks, np);
		return(blocks);
	}

	for(c1 = np; *c1; ++c1);
	if(*(c1-1) == '/')
		--c1;
	endofname = c1;
	dirsize = Statb.st_size;
	if(chdir(fname) == -1)
		return 0;
	for(offset=0; offset < dirsize; offset += BUFSIZ) { /* each block */
		dsize = BUFSIZ<(dirsize-offset)? BUFSIZ: (dirsize-offset);
		if(!dir) {
			if((dir=open(".",0))<0) {
				fprintf(stderr, "--cannot open < %s >\n",
					np);
				goto ret;
			}
			if(offset) lseek(dir, (long)offset, 0);
			if(read(dir, (char *)dentry, dsize)<0) {
				fprintf(stderr, "--cannot read < %s >\n",
					np);
				goto ret;
			}
			if(dir > 10) {
				close(dir);
				dir = 0;
			}
		} else 
			if(read(dir, (char *)dentry, dsize)<0) {
				fprintf(stderr, "--cannot read < %s >\n",
					np);
				goto ret;
			}
		for(dp=dentry, entries=dsize>>4; entries; --entries, ++dp) {
			/* each directory entry */
			if(dp->d_ino==0
			|| EQ(dp->d_name, ".")
			|| EQ(dp->d_name, ".."))
				continue;
			c1 = endofname;
			*c1++ = '/';
			c2 = dp->d_name;
			for(i=0; i<DIRSIZ; ++i)
				if(*c2)
					*c1++ = *c2++;
				else
					break;
			*c1 = '\0';
			if(c1 == endofname) /* ?? */
				return 0L;
			blocks += descend(np, endofname+1);
		}
	}
	*endofname = '\0';
	if(!Sflag)
		printf("%ld	%s\n", blocks, np);
ret:
	if(dir)
		close(dir);
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
