#ifndef lint
static char sccsid[] = "@(#)getgrnam.c	5.1 (Berkeley) %G%";
#endif not lint

#include <grp.h>

struct group *
getgrnam(name)
register char *name;
{
	register struct group *p;
	struct group *getgrent();

	setgrent();
	while( (p = getgrent()) && strcmp(p->gr_name,name) );
	endgrent();
	return(p);
}
