/* refile.c - refile the draft into another folder */
#ifndef	lint
static char ident[] = "@(#)$Id: refile.c,v 1.2 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>

int     refile (arg, file)
register char **arg,
               *file;
{
    int     pid;
    register int    vecp;
    char   *vec[MAXARGS];

    vecp = 0;
    vec[vecp++] = r1bindex (fileproc, '/');
    vec[vecp++] = "-nolink";	/* override bad .mh_profile defaults */
    vec[vecp++] = "-nopreserve";
    vec[vecp++] = "-file";
    vec[vecp++] = file;

    if (arg)
	while (*arg)
	    vec[vecp++] = *arg++;
    vec[vecp] = NULL;

    m_update ();
    (void) fflush (stdout);

    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return NOTOK;

	case OK: 
	    execvp (fileproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (fileproc);
	    _exit (-1);

	default: 
	    return (pidwait (pid, NOTOK));
    }
}
