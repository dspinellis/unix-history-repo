#ifndef lint
static char sccsid[] = "@(#)enroll.c	4.3 %G%";
#endif

#include <sys/types.h>
#include <pwd.h>
#include "xmail.h"
#include "pathnames.h"

MINT *a[42], *x, *b, *one, *c64, *t45, *z, *q, *r, *two, *t15;
char buf[256];
char maildir[] = _PATH_SECRETMAIL;
main()
{
	int uid, i;
	FILE *fd;
	char *myname, fname[128];
	uid = getuid();
	myname = (char *) getlogin();
	if(myname == NULL)
		myname = getpwuid(uid)->pw_name;
	sprintf(fname, "%s/%s.key", maildir, myname);
	comminit();
	setup(getpass("Gimme key: "));
	mkb();
	mkx();
#ifdef debug
	omout(b);
	omout(x);
#endif
	mka();
	i = creat(fname, 0644);
	if(i<0)
	{	perror(fname);
		exit(1);
	}
	close(i);
	fd = fopen(fname, "w");
	for(i=0; i<42; i++)
		nout(a[i], fd);
	exit(0);
}
