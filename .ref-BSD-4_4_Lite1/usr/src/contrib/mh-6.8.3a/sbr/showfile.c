/* showfile.c - invoke lproc */

#include "../h/mh.h"
#include <stdio.h>


showfile (arg, file)
register char **arg,
               *file;
{
    int     isdraft,
            pid;
    register int    vecp;
    char   *vec[MAXARGS];

    m_update ();
    (void) fflush (stdout);

    if (strcmp (r1bindex (lproc, '/'), "mhl") == 0)
	lproc = mhlproc;

    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return 1;

	case OK: 
	    vecp = 0;
	    vec[vecp++] = r1bindex (lproc, '/');
	    isdraft = 1;
	    if (arg)
		while (*arg) {
		    if (**arg != '-')
			isdraft = 0;
		    vec[vecp++] = *arg++;
		}
	    if (isdraft) {
		if (strcmp (vec[0], "show") == 0)
		    vec[vecp++] = "-file";
		vec[vecp++] = file;
	    }
	    vec[vecp] = NULL;

	    execvp (lproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (lproc);
	    _exit (-1);

	default: 
	    return (pidwait (pid, NOTOK) & 0377 ? 1 : 0);
    }
}
