#ifndef lint
static char sccsid[] = "@(#)getwd.c	5.5 (Berkeley) 6/29/90";
#endif

#include "uucp.h"

/*
 *	get working directory
 *
 *	return codes  0 = FAIL
 *		      wkdir = SUCCES
 */

char *
getwd(wkdir)
register char *wkdir;
{
	register FILE *fp;
	extern FILE *rpopen();
	extern int rpclose();
	register char *c;

	*wkdir = '\0';
	if ((fp = rpopen("PATH=/bin:/usr/bin;pwd 2>&-", "r")) == NULL)
		return 0;
	if (fgets(wkdir, 100, fp) == NULL) {
		rpclose(fp);
		return 0;
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	rpclose(fp);
	return wkdir;
}
