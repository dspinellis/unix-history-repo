/* makedir.c - make a directory */

#include "../h/mh.h"
#include <stdio.h>


makedir (dir)
register char *dir;
{
    int     pid;
    register char  *cp;

    m_update ();
    (void) fflush (stdout);

#ifdef	BSD42
    if (getuid () == geteuid ()) {
	if (mkdir (dir, 0755) == NOTOK) {
	    advise (dir, "unable to create directory");
	    return 0;
	}
    }
    else
#endif	BSD42
    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return 0;

	case OK: 
	    (void) setgid (getgid ());
	    (void) setuid (getuid ());

	    execl ("/bin/mkdir", "mkdir", dir, NULLCP);
	    execl ("/usr/bin/mkdir", "mkdir", dir, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror ("mkdir");
	    _exit (-1);

	default: 
	    if (pidXwait (pid, "mkdir"))
		return 0;
	    break;
    }

    if ((cp = m_find ("folder-protect")) == NULL)
	cp = foldprot;
    (void) chmod (dir, atooi (cp));
    return 1;
}

