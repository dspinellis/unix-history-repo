#include "stdio.h"
#include "lrnref"
#define	ND	64

start(lesson)
char *lesson;
{
	struct direct {
		int inode; 
		char name[14];
	};
	struct direct dv[ND], *dm, *dp;
	int f, c, n;
	char where [100];

	f = open(".", 0);
	n = read(f, dv, ND*sizeof(*dp));
	n /= sizeof(*dp);
	if (n==ND)
		fprintf(stderr, "lesson too long\n");
	dm = dv+n;
	for(dp=dv; dp<dm; dp++)
		if (dp->inode) {
			n = strlen(dp->name);
			if (dp->name[n-2] == '.' && dp->name[n-1] == 'c')
				continue;
			c = dp->name[0];
			if (c>='a' && c<= 'z')
				unlink(dp->name);
		}
	close(f);
	if (ask)
		return;
	sprintf(where, "../../%s/L%s", sname, lesson);
	if (access(where, 04)==0)	/* there is a file */
		return;
	fprintf(stderr, "No lesson %s\n",lesson);
	wrapup(1);
}

fcopy(new,old)
char *new, *old;
{
	char b[512];
	int n, fn, fo;
	fn = creat(new, 0666);
	fo = open(old,0);
	if (fo<0) return;
	if (fn<0) return;
	while ( (n=read(fo, b, 512)) > 0)
		write(fn, b, n);
	close(fn);
	close(fo);
}
