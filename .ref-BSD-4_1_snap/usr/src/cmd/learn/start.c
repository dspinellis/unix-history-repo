#include "stdio.h"
#include <sys/types.h>
#include <sys/dir.h>
#include "lrnref"
#define	ND	64

start(lesson)
char *lesson;
{
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
		if (dp->d_ino) {
			n = strlen(dp->d_name);
			if (dp->d_name[n-2] == '.' && dp->d_name[n-1] == 'c')
				continue;
			c = dp->d_name[0];
			if (c>='a' && c<= 'z')
				unlink(dp->d_name);
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
	char b[BUFSIZ];
	int n, fn, fo;
	fn = creat(new, 0666);
	fo = open(old,0);
	if (fo<0) return;
	if (fn<0) return;
	while ( (n=read(fo, b, BUFSIZ)) > 0)
		write(fn, b, n);
	close(fn);
	close(fo);
}
