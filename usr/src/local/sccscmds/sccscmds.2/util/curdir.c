/*
 * SCCSID(@(#)curdir.c	4.4);
 */
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

#define	dot	"."
#define	dotdot	".."

static	char	*name;

static	int	off	= -1;
static	struct	stat	d, dd;
static	struct	direct	*dir;
static	DIR	*dirp;
static	int	cat(), prexit();

char *
curdir(np)
char *np;
{
	int rdev, rino;

	*np++ = '/';
	name = np;
	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;) {
		stat(dot, &d);
		if (d.st_ino==rino && d.st_dev==rdev)
			goto done;
		if ((dirp = opendir(dotdot)) == 0)
			prexit("curdir: cannot open ..\n");
		fstat(dirp->dd_fd, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				goto done;
			do
				if ((dir = readdir(dirp)) == 0)
					prexit("curdir: read error in ..\n");
			while (dir->d_ino != d.st_ino);
		} else
			do {
				if ((dir = readdir(dirp)) == 0)
					prexit("curdir: read error in ..\n");
				stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		closedir(dirp);
		cat();
	}
done:
	name--;
	if (chdir(name) < 0) {
		write(2, name, strlen(name));
		prexit(": can't change back\n");
	}
	return (0);
}

static
cat()
{
	register i, j;

	i = dir->d_namlen;
	if ((off+i+2) > 1024-1)
		return;
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	if (off >= 0)
		name[i] = '/';
	off += i+1;
	name[off] = 0;
	for(--i; i>=0; --i)
		name[i] = dir->d_name[i];
}

static
prexit(cp)
char *cp;
{
	write(2, cp, strlen(cp));
	exit(1);
}
