/* remdir.c - remove a directory */
#ifndef	lint
static char ident[] = "@(#)$Id: remdir.c,v 1.5 1992/12/15 00:20:22 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include <stdio.h>


remdir (dir)
char *dir;
{
#if	!defined (BSD42) && !defined (SYS5DIR)
    int     pid;
#endif	/* not BSD42 and not SYS5DIR */

    m_update ();
    (void) fflush (stdout);

#if	!defined (BSD42) && !defined (SYS5DIR)
    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return 0;

	case OK: 
	    execl ("/bin/rmdir", "rmdir", dir, NULLCP);
	    execl ("/usr/bin/rmdir", "rmdir", dir, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror ("rmdir");
	    _exit (-1);

	default: 
	    if (pidXwait (pid, "rmdir"))
		return 0;
	    break;
    }
#else	/* BSD42 or SYS5DIR */
    if (rmdir (dir) == NOTOK) {
	admonish (dir, "unable to remove directory");
	return 0;
    }
#endif	/* BSD42 or SYS5DIR */

    return 1;
}
