/*
 * Print working (current) directory
 */
#include	<stdio.h>
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

char	dot[]	= ".";
char	dotdot[] = "..";
char	name[BUFSIZ];
int	file;
int	off	= -1;
struct	stat	d, dd;
struct	direct	dir;

main()
{
	int rdev, rino;

	stat("/", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;) {
		stat(dot, &d);
		if (d.st_ino==rino && d.st_dev==rdev)
			prname();
		if ((file = open(dotdot,0)) < 0) {
			fprintf(stderr,"pwd: cannot open ..\n");
			exit(1);
		}
		fstat(file, &dd);
		chdir(dotdot);
		if(d.st_dev == dd.st_dev) {
			if(d.st_ino == dd.st_ino)
				prname();
			do
				if (read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
					fprintf(stderr,"read error in ..\n");
					exit(1);
				}
			while (dir.d_ino != d.st_ino);
		}
		else do {
				if(read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
					fprintf(stderr,"read error in ..\n");
					exit(1);
				}
				stat(dir.d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		close(file);
		cat();
	}
}

prname()
{
	write(1, "/", 1);
	if (off<0)
		off = 0;
	name[off] = '\n';
	write(1, name, off+1);
	exit(0);
}

cat()
{
	register i, j;

	i = -1;
	while (dir.d_name[++i] != 0);
	if ((off+i+2) > BUFSIZ-1)
		prname();
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	off=i+off+1;
	name[i] = '/';
	for(--i; i>=0; --i)
		name[i] = dir.d_name[i];
}
