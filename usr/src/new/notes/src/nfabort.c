#include	<stdio.h>

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfabort.c,v 1.7 85/01/18 15:40:50 notes Rel $";
#endif	RCSIDENT

/*
 *	nfabort(nf, message, title, cname, exitcode)
 *
 *	Dump a core file and leave it in "cname" suffixed with
 *	the pid of the current process.  Place "message" and
 *	a line about where the core dump is in "nf" with "title"
 *	and then terminate with "exitcode" as an exit code.
 *
 *	Ray Essick,	February 1984
 */

extern char *malloc ();

nfabort (nf, message, title, cname, exitcode)
char   *nf;
char   *message;
char   *title;
char   *cname;
int     exitcode;
{
    int     mypid;
    int     pid;
    int     rpid;
    int     retcode;
    char    pbuf[256];					/* char buffer */
    char   *p;						/* for message */

    if (message == NULL)				/* empty */
	message = "";					/* fake one */
    if (cname == NULL || *cname == '\0')		/* make one */
	cname = "core";
    if (title == NULL || *title == '\0')		/* title */
	title = "nfabort";
    mypid = getpid ();

    switch (pid = fork ())
    {
	case -1: 					/* couldn't fork! */
	    fprintf (stderr, "nfabort() unable to log dump\n");
	    fflush (stderr);
							/* dump core anyway */
	case 0: 					/* child */
	    umask (0);
	    abort ();					/* die quick */

	default: 					/* parent */
	    while ((rpid = wait (&retcode)) != pid && rpid != -1);
	    sprintf (pbuf, "/bin/mv core %s.%d", cname, mypid);
	    system (pbuf);				/* move it */
	    sprintf (pbuf, "/bin/chmod 666 %s.%d", cname, mypid);
	    system (pbuf);				/* un-protect it */
	    sprintf (pbuf, "Core image left in %s.%d\n", cname, mypid);
	    p = malloc (strlen (message) + strlen (pbuf) + 4);
	    if (p == NULL)				/* no space */
		p = message;				/* write something */
	    else
		sprintf (p, "%s\n\n%s", message, pbuf);
	    if (nf)					/* only if given */
		nfcomment (nf, p, title, 0, 0);		/* and log it */
	    exit (exitcode);
    }
}
