#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrgid.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <grp.h>

struct group *
getgrgid(gid)
register gid;
{
	register struct group *p;
	struct group *getgrent();

	setgrent();
	while( (p = getgrent()) && p->gr_gid != gid );
	endgrent();
	return(p);
}
