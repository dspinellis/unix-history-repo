#ifndef lint
static char sccsid[] = "@(#)start.c	4.6	(Berkeley)	%G%";
#endif not lint

#include "stdio.h"
#include "lrnref.h"
#include <sys/types.h>
#include <dirent.h>

start(lesson)
char *lesson;
{
	struct dirent dbuf;
	register struct dirent *ep = &dbuf;	/* directory entry pointer */
	int c, n;
	char where [100];
	DIR *dp;

#define OPENDIR(s)	((dp = opendir(s)) != NULL)
#define DIRLOOP(s)	for (s = readdir(dp); s != NULL; s = readdir(dp))
#define EPSTRLEN	ep->d_namlen
#define CLOSEDIR	closedir(dp)

	if (!OPENDIR(".")) {		/* clean up play directory */
		perror("Start:  play directory");
		wrapup(1);
	}
	DIRLOOP(ep) {
		if (ep->d_ino == 0)
			continue;
		n = EPSTRLEN;
		if (ep->d_name[n-2] == '.' && ep->d_name[n-1] == 'c')
			continue;
		c = ep->d_name[0];
		if (c>='a' && c<= 'z')
			unlink(ep->d_name);
	}
	CLOSEDIR;
	if (ask)
		return;
	sprintf(where, "%s/%s/L%s", direct, sname, lesson);
	if (access(where, 04)==0)	/* there is a file */
		return;
	perror(where);
	fprintf(stderr, "Start:  no lesson %s\n",lesson);
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
