#ifndef lint
static char sccsid[] = "@(#)cpmv.c	5.5 (Berkeley) 10/9/85";
#endif

#include "uucp.h"
#include <sys/stat.h>

/*LINTLIBRARY*/

/*
 *	copy f1 to f2
 *
 *	return - SUCCESS | FAIL
 */
xcp(f1, f2)
char *f1, *f2;
{
	char buf[BUFSIZ];
	register int len;
	register int fp1, fp2;
	char *lastpart();
	char full[MAXFULLNAME];
	struct stat s;

	if ((fp1 = open(subfile(f1), 0)) < 0)
		return FAIL;
	strcpy(full, f2);
	if (stat(subfile(f2), &s) == 0) {
		/* check for directory */
		if ((s.st_mode & S_IFMT) == S_IFDIR) {
			strcat(full, "/");
			strcat(full, lastpart(f1));
		}
	}
	DEBUG(4, "full %s\n", full);
	if ((fp2 = creat(subfile(full), 0666)) < 0) {
		close(fp1);
		return FAIL;
	}
	while((len = read(fp1, buf, BUFSIZ)) > 0)
		 if (write(fp2, buf, len) != len) {
			len = -1;
			break;
		}

	close(fp1);
	close(fp2);
	return len < 0 ? FAIL: SUCCESS;
}


/*
 *	move f1 to f2
 *
 *	return  0 ok  |  FAIL failed
 */
xmv(f1, f2)
register char *f1, *f2;
{
	register int ret;

	(void) unlink(subfile(f2));
	if (link(subfile(f1), subfile(f2)) < 0) {
		/*  copy file  */
		ret = xcp(f1, f2);
		if (ret == 0)
			unlink(subfile(f1));
		return ret;
	}
	(void) unlink(subfile(f1));
	return 0;
}
