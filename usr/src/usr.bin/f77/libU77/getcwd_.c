/** F77 NOTE: the getcwd() routine should be in libc.a ! **/
/*
 * return name of working (current) directory
 */
#include	<stdio.h>
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

static char	dot[]	= ".";
static char	dotdot[] = "..";
static char	name[128];

char *
getcwd()
{
	int	rdev, rino;
	int	fd;
	struct	stat	d, dd;
	struct	direct	dir;
	char	*prepend();
	char	*namep = &name[(sizeof name)-1];

	*namep = '\0';
	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;)
	{
		stat(dot, &d);
		if (d.st_ino == rino && d.st_dev == rdev)
		{
			chdir(namep);
			return(namep);
		}
		if ((fd = open(dotdot,0)) < 0)
		{
			chdir(prepend(namep, dot));
			return((char *)0);
		}
		chdir(dotdot);
		fstat(fd, &dd);
		if(d.st_dev == dd.st_dev)
		{
			if(d.st_ino == dd.st_ino)
			{
				close(fd);
				chdir(namep);
				return(namep);
			}
			do
			{
				if (read(fd, (char *)&dir, sizeof(dir)) < sizeof(dir))
				{
					close(fd);
					chdir(prepend(namep, dot));
					return((char *)0);
				}
			} while (dir.d_ino != d.st_ino);
		}
		else do
		{
				if(read(fd, (char *)&dir, sizeof(dir)) < sizeof(dir))
				{
					close(fd);
					chdir(prepend(namep, dot));
					return((char *)0);
				}
				stat(dir.d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		close(fd);
		namep = prepend(prepend(namep, dir.d_name), "/");
	}
}

char *
prepend(p, n)
char *p;
char *n;
{
	int i = 0;

	while (i < DIRSIZ && *n)
	{
		n++; i++;
	}
	while (i--)
		*--p = *--n;
	return(p);
}

/*
char id_getcwd[] = "@(#)getcwd_.c	1.2";
 * Get pathname of current working directory.
 *
 * calling sequence:
 *	character*128 path
 *	ierr = getcwd(path)
 * where:
 *	path will receive the pathname of the current working directory.
 *	ierr will be 0 if successful, a system error code otherwise.
 */

extern int errno;

long
getcwd_(path, len)
char *path;
long len;
{
	char *p;

	p = getcwd();
	if (p)
	{
		b_char(p, path, len);
		return(0L);
	}
	return((long)errno);
}
