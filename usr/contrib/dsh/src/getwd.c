/*
 *  Getwd
 */
#include	<stdio.h>
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

char	dot[]	= ".";
char	dotdot[] = "..";
DIR	*file;
int	off	= -1;
struct	stat	d, dd;
struct	direct	*dir;

getwd(name)
char	*name;
{
	int rdev, rino;

	name[0] = '/';
	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;) {
		stat(dot, &d);
		if (d.st_ino==rino && d.st_dev==rdev) {
			return;
		}
		if ((file = opendir(dotdot)) == NULL) {
			error ("can't open ..");
		}
		fstat(file->dd_fd, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino){
				return;
			}
			do
				if ((dir = readdir(file)) == NULL) {
					fprintf(stderr,"read error in ..\n");
					exit(1);
				}
			while (dir->d_ino != d.st_ino);
		} else
			do {
				if ((dir = readdir(file)) == NULL) {
					fprintf(stderr,"read error in ..\n");
					exit(1);
				}
				stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		closedir(file);
		cat(name+1);
	}
}

cat(name)
char *name;
{
	register i, j;

	i = dir->d_namlen;
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	off=i+off+1;
	name[i] = '/';
	for(--i; i>=0; --i)
		name[i] = dir->d_name[i];
}
