/* mhmail.c - simple mail program */
#ifndef	lint
static char ident[] = "@(#)$Id: mhmail.c,v 1.9 1993/08/20 15:51:29 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	BODYSW	0
    "body text", 0,

#define	CCSW	1
    "cc addrs ...", 0,

#define	FROMSW	2
    "from addr", 0,

#define	SUBJSW	3
    "subject text", 0,

#define	HELPSW	4
    "help", 4,

#define	RESNDSW	5
    "resent", -6,
#define	QUEUESW	6
    "queued", -6,

    NULL, 0
};

/*  */

static TYPESIG	intrser ();


static char tmpfil[BUFSIZ];

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     child_id,
	    status,
            i,
            iscc = 0,
	    nvec,
	    queued = 0,
	    resent = 0,
            somebody;
    char   *cp,
           *tolist = NULL,
           *cclist = NULL,
           *subject = NULL,
	   *from = NULL,
           *body = NULL,
          **argp = argv + 1,
            buf[100],
	   *vec[5];
    FILE * out;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    m_foil (NULLCP);

    if (argc == 1) {
	execlp (incproc, r1bindex (incproc, '/'), NULLCP);
	adios (incproc, "unable to exec");
    }

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);

		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);

		case HELPSW: 
		    (void) sprintf (buf, "%s [addrs ... [switches]]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case FROMSW: 
		    if (!(from = *argp++) || *from == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case BODYSW: 
		    if (!(body = *argp++) || *body == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case CCSW: 
		    iscc++;
		    continue;

		case SUBJSW: 
		    if (!(subject = *argp++) || *subject == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case RESNDSW: 
		    resent++;
		    continue;

		case QUEUESW: 
		    queued++;
		    continue;
	    }
	if (iscc)
	    cclist = cclist ? add (cp, add (", ", cclist)) : getcpy (cp);
	else
	    tolist = tolist ? add (cp, add (", ", tolist)) : getcpy (cp);
    }

/*  */

    if (tolist == NULL)
	adios (NULLCP, "usage: %s addrs ... [switches]", invo_name);
    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((out = fopen (tmpfil, "w")) == NULL)
	adios (tmpfil, "unable to write");
    (void) chmod (tmpfil, 0600);

    setsig (SIGINT, intrser);

    fprintf (out, "%sTo: %s\n", resent ? "Resent-" : "", tolist);
    if (cclist)
	fprintf (out, "%scc: %s\n", resent ? "Resent-" : "", cclist);
    if (subject)
	fprintf (out, "%sSubject: %s\n", resent ? "Resent-" : "", subject);
    if (from)
	fprintf (out, "%sFrom: %s\n", resent ? "Resent-" : "", from);
    if (!resent)
	(void) fputs ("\n", out);

    if (body) {
	fprintf (out, "%s", body);
	if (*body && *(body + strlen (body) - 1) != '\n')
	    fputs ("\n", out);
    }
    else {
	for (somebody = 0;
		(i = fread (buf, sizeof *buf, sizeof buf, stdin)) > 0;
		somebody++)
	    if (fwrite (buf, sizeof *buf, i, out) != i)
		adios (tmpfil, "error writing");
	if (!somebody) {
	    (void) unlink (tmpfil);
	    done (1);
	}
    }
    (void) fclose (out);

/*  */

    nvec = 0;
    vec[nvec++] = r1bindex (postproc, '/');
    vec[nvec++] = tmpfil;
    if (resent)
	vec[nvec++] = "-dist";
    if (queued)
	vec[nvec++] = "-queued";
    vec[nvec] = 0;

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 		/* report failure and then send it */
	    admonish (NULLCP, "unable to fork");

	case OK:
	    execvp (postproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (postproc);
	    _exit (-1);

	default: 
	    if (status = pidXwait (child_id, postproc)) {
		fprintf (stderr, "Letter saved in dead.letter\n");
		execl ("/bin/mv", "mv", tmpfil, "dead.letter", NULLCP);
		execl ("/usr/bin/mv", "mv", tmpfil, "dead.letter", NULLCP);
		perror ("mv");
		_exit (-1);
	    }

	    (void) unlink (tmpfil);
	    done (status ? 1 : 0);
    }
}

/*  */

/* ARGSUSED */

static TYPESIG  intrser (i)
int     i;
{
#ifndef	BSD42
    if (i)
	(void) signal (i, SIG_IGN);
#endif	/* BSD42 */

    (void) unlink (tmpfil);
    done (i != 0 ? 1 : 0);
}
