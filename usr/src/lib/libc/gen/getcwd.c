/*	@(#)getcwd.c	4.3	(Berkeley)	%G%	*/

/*
 * Getwd
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

#define	dot	"."
#define	dotdot	".."

static	char *name;

static	DIR *file;
static	int off;
static	struct stat d, dd;
static	struct direct *dir;

char *
getwd(np)
	char *np;
{
	dev_t rdev;
	ino_t rino;

	off = -1;
	*np++ = '/';
	name = np;
	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;) {
		stat(dot, &d);
		if (d.st_ino==rino && d.st_dev==rdev)
			goto done;
		if ((file = opendir(dotdot)) == NULL)
			prexit("getwd: cannot open ..\n");
		fstat(file->dd_fd, &dd);
		if (chdir(dotdot) < 0)
			prexit("getwd: cannot chdir to ..\n");
		if (d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				goto done;
			do
				if ((dir = readdir(file)) == NULL)
					prexit("getwd: read error in ..\n");
			while (dir->d_ino != d.st_ino);
		} else
			do {
				if ((dir = readdir(file)) == NULL)
					prexit("getwd: read error in ..\n");
				stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		closedir(file);
		cat();
	}
done:
	name--;
	if (chdir(name) < 0)
		prexit("getwd: can't change back\n");
	return (name);
}

cat()
{
	register i, j;

	i = -1;
	while (dir->d_name[++i] != 0);
	if ((off+i+2) > 1024-1)
		return;
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	if (off >= 0)
		name[i] = '/';
	off=i+off+1;
	name[off] = 0;
	for(--i; i>=0; --i)
		name[i] = dir->d_name[i];
}

prexit(cp)
	char *cp;
{
	write(2, cp, strlen(cp));
	exit(1);
}
