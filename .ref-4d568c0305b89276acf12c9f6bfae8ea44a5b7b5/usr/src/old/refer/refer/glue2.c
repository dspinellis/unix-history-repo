/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)glue2.c	4.3 (Berkeley) %G%";
#endif /* not lint */

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
