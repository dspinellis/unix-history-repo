/* remdir.c - remove a directory */

#include "../h/mh.h"
#include <stdio.h>


remdir (dir)
char *dir;
{
#ifndef	BSD42
    int     pid;
#endif	not BSD42

    m_update ();
    (void) fflush (stdout);

#ifndef	BSD42
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
#else	BSD42
    if (rmdir (dir) == NOTOK) {
	admonish (dir, "unable to remove directory");
	return 0;
    }
#endif	BSD42

    return 1;
}
