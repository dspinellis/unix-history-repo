#ifndef lint
static char *sccsid = "@(#)glue2.c	4.2 (Berkeley) 5/11/89";
#endif

#include "pathnames.h"
char refdir[50];

savedir()
{
	if (refdir[0]==0)
		corout ("", refdir, _PATH_PWD, "", 50);
	trimnl(refdir);
}

restodir()
{
	chdir(refdir);
}
