/*
 * Print working (current) directory
 */
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/dir.h>

char	dot[]	".";
char	dotdot[]	"..";
char	root[]	"/";
char	name[512];
int	file;
int	off	-1;
struct	stat	x;
struct	direct	y;

main()
{
	for (;;) {
		stat(dot, &x);
		if ((file = open(dotdot,0)) < 0) prname();
		do {
			if (read(file, &y, sizeof(y)) < sizeof(y))
				prname();
		} while (y.d_ino != x.st_ino);
		close(file);
		if (y.d_ino == 2)
			ckroot();
		cat();
		chdir(dotdot);
	}
}

ckroot()
{
	register i;

	if (stat(y.d_name,&x)<0 || chdir(root)<0 || (file=open(root,0))<0)
		prname();
	i = x.st_dev;
	do {
		if (read(file,&y,sizeof(y)) < sizeof(y))
			prname();
		if (y.d_ino == 0)
			continue;
		if (stat(y.d_name,&x) < 0)
			prname();
	} while (x.st_dev!=i || (x.st_mode&S_IFMT)!=S_IFDIR);
	if (strcmp(dot, y.d_name) || strcmp(dotdot, y.d_name))
		cat();
	write(1, root, 1);
	prname();
}

prname()
{
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
	while (y.d_name[++i] != 0);
	if ((off+i+2) > 511)
		prname();
	for(j=off+1; j>=0; --j)
		name[j+i+1] = name[j];
	off=i+off+1;
	name[i] = root[0];
	for(--i; i>=0; --i)
		name[i] = y.d_name[i];
}
