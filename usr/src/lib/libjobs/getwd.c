/*
 * Getwd
 */
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

#define	dot	"."
#define	dotdot	".."

static	char	*name;

static	int	file;
static	int	off	= -1;
static	struct	stat	d, dd;
static	struct	direct	dir;

char *
getwd(np)
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
		if ((file = open(dotdot,0)) < 0)
			prexit("getwd: cannot open ..\n");
		fstat(file, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				goto done;
			do
				if (read(file, (char *)&dir, sizeof(dir)) < sizeof(dir))
					prexit("getwd: read error in ..\n");
			while (dir.d_ino != d.st_ino);
		}
		else do {
				if(read(file, (char *)&dir, sizeof(dir)) < sizeof(dir))
					prexit("getwd: read error in ..\n");
				stat(dir.d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		close(file);
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
	while (dir.d_name[++i] != 0);
	if ((off+i+2) > 1024-1)
		return;
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	if (off >= 0)
		name[i] = '/';
	off=i+off+1;
	name[off] = 0;
	for(--i; i>=0; --i)
		name[i] = dir.d_name[i];
}

prexit(cp)
char *cp;
{
	write(2, cp, strlen(cp));
	exit(1);
}
