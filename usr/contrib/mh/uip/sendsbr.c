/* sendsbr.c - routines to help WhatNow/Send along */

#include "../h/mh.h"
#include <setjmp.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */

int     debugsw = 0;		/* global */
int     forwsw = 1;
int     inplace = 0;
int     pushsw = 0;
int     unique = 0;

char   *altmsg = NULL;		/*  .. */
char   *annotext = NULL;
char   *distfile = NULL;


static int armed = 0;
static jmp_buf env;


char   *getusr ();
long	lseek ();

/*  */

int	sendsbr (vec, vecp, drft, st)
register char **vec,
	       *drft;
int     vecp;
register struct stat *st;
{
    int     status;

    armed++;
    switch (setjmp (env)) {
	case OK: 
	    status = sendaux (vec, vecp, drft, st) ? NOTOK : OK;
	    break;

	default: 
	    status = DONE;
	    break;
    }
    armed = 0;
    if (distfile)
	(void) unlink (distfile);

    return status;
}

/*  */

int	sendaux (vec, vecp, drft, st)
register char **vec,
	       *drft;
int     vecp;
register struct stat *st;
{
    int     child_id,
            i,
	    status,
            fd,
            fd2;
    char    backup[BUFSIZ],
            buf[BUFSIZ],
            file[BUFSIZ];

    fd = pushsw ? tmp_fd () : NOTOK;
    fd2 = NOTOK;

    if (pushsw && unique) {
	if (rename (drft, strcpy (file, m_scratch (drft, invo_name)))
		== NOTOK)
	    adios (file, "unable to rename %s to", drft);
	drft = file;
    }
    vec[vecp++] = drft;
    if (annotext)
	if ((fd2 = tmp_fd ()) != NOTOK) {
	    vec[vecp++] = "-idanno";
	    (void) sprintf (buf, "%d", fd2);
	    vec[vecp++] = buf;
	}
	else
	    admonish (NULLCP, "unable to create file for annotation list");
    if (distfile && distout (drft, distfile, backup) == NOTOK)
	done (1);
    vec[vecp] = NULL;

    for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 		/* oops */
	    adios ("fork", "unable to");

	case OK: 		/* send it */
	    if (fd != NOTOK) {
		(void) dup2 (fd, fileno (stdout));
		(void) dup2 (fd, fileno (stderr));
		(void) close (fd);
	    }
	    execvp (postproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (postproc);
	    _exit (-1);

	default: 		/* wait for it */
	    if ((status = pidwait (child_id, NOTOK)) == 0) {
		if (annotext && fd2 != NOTOK)
		    anno (fd2, st);
		if (rename (drft, strcpy (buf, m_backup (drft))) == NOTOK)
		    advise (buf, "unable to rename %s to", drft);
	    }
	    else {
		if (fd != NOTOK) {
		    alert (drft, fd);
		    (void) close (fd);
		}
		else
		    advise (NULLCP, "message not delivered to anyone");
		if (fd2 != NOTOK)
		    (void) close (fd2);
		if (distfile) {
		    (void) unlink (drft);
		    if (rename (backup, drft) == NOTOK)
			advise (drft, "unable to rename %s to", backup);
		}
	    }
	    break;
    }

    return status;
}

/*  */

static  alert (file, out)
register char   *file;
int     out;
{
    int     child_id,
            i,
            in;
    char    buf[BUFSIZ];

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 		/* oops */
	    advise ("fork", "unable to");

	case OK: 		/* send it */
	    (void) signal (SIGHUP, SIG_IGN);
	    (void) signal (SIGINT, SIG_IGN);
	    (void) signal (SIGQUIT, SIG_IGN);
	    (void) signal (SIGTERM, SIG_IGN);
	    if (forwsw)
		if ((in = open (file, 0)) == NOTOK)
		    admonish (file, "unable to re-open");
		else {
		    (void) lseek (out, 0L, 2);
		    (void) strcpy (buf, "\nMessage not delivered to anyone.\n");
		    (void) write (out, buf, strlen (buf));
		    (void) strcpy (buf, "\n------- Unsent Draft\n\n");
		    (void) write (out, buf, strlen (buf));
		    cpydgst (in, out, file, "temporary file");
		    (void) close (in);
		    (void) strcpy (buf, "\n------- End of Unsent Draft\n");
		    (void) write (out, buf, strlen (buf));
		    if (rename (file, strcpy (buf, m_backup (file))) == NOTOK)
			admonish (buf, "unable to rename %s to", file);
		}
	    (void) lseek (out, 0L, 0);
	    (void) dup2 (out, fileno (stdin));
	    (void) close (out);
	    (void) sprintf (buf, "send failed on %s",
			forwsw ? "enclosed draft" : file);

	    execlp (mailproc, r1bindex (mailproc, '/'), getusr (),
		    "-subject", buf, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (mailproc);
	    _exit (-1);

	default: 		/* no waiting... */
	    break;
    }
}

/*  */

static int  tmp_fd () {
    int     fd;
    char    tmpfil[BUFSIZ];

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	return NOTOK;
    (void) close (fd);

    if ((fd = open (tmpfil, 2)) == NOTOK)
	return NOTOK;
    if (debugsw)
	advise (NULLCP, "temporary file %s selected", tmpfil);
    else
	if (unlink (tmpfil) == NOTOK)
	    advise (tmpfil, "unable to remove");

    return fd;
}

/*  */

static anno (fd, st)
int	fd;
register struct stat *st;
{
    int     child_id;
    int     (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
    static char *cwd = NULL;
    struct stat st2;

    if (altmsg &&
	    (stat (altmsg, &st2) == NOTOK
		|| st -> st_mtime != st2.st_mtime
		|| st -> st_dev != st2.st_dev
		|| st -> st_ino != st2.st_ino)) {
	if (debugsw)
	    admonish (NULLCP, "$mhaltmsg mismatch");
	return;
    }

    child_id = debugsw ? NOTOK : fork ();
    switch (child_id) {
	case NOTOK: 		/* oops */
	    if (!debugsw)
		advise (NULLCP,
			    "unable to fork, so doing annotations by hand...");
	    if (cwd == NULL)
		cwd = getcpy (pwd ());

	case OK: 
	    hstat = signal (SIGHUP, SIG_IGN);
	    istat = signal (SIGINT, SIG_IGN);
	    qstat = signal (SIGQUIT, SIG_IGN);
	    tstat = signal (SIGTERM, SIG_IGN);
	    annoaux (fd);
	    if (child_id == OK)
		_exit (0);
	    (void) signal (SIGHUP, hstat);
	    (void) signal (SIGINT, istat);
	    (void) signal (SIGQUIT, qstat);
	    (void) signal (SIGTERM, tstat);
	    (void) chdir (cwd);
	    break;

	default: 		/* no waiting... */
	    (void) close (fd);
	    break;
    }
}

/*  */

static	annoaux (fd)
int	fd;
{
    int	    fd2,
	    fd3,
	    msgnum;
    char   *cp,
	   *folder,
	   *maildir,
            buffer[BUFSIZ],
          **ap;
    FILE   *fp;
    struct msgs *mp;

    if ((folder = getenv ("mhfolder")) == NULL || *folder == NULL) {
	if (debugsw)
	    admonish (NULLCP, "$mhfolder not set");
	return;
    }
    maildir = m_maildir (folder);
    if (chdir (maildir) == NOTOK) {
	if (debugsw)
	    admonish (maildir, "unable to change directory to");
	return;
    }
    if (!(mp = m_gmsg (folder))) {
	if (debugsw)
	    admonish (NULLCP, "unable to read folder %s");
	return;
    }
    if (mp -> hghmsg == 0) {
	if (debugsw)
	    admonish (NULLCP, "no messages in %s", folder);
	goto oops;
    }

    if ((cp = getenv ("mhmessages")) == NULL || *cp == NULL) {
	if (debugsw)
	    admonish (NULLCP, "$mhmessages not set");
	goto oops;
    }
    if (!debugsw			/* MOBY HACK... */
	    && pushsw
	    && (fd3 = open ("/dev/null", 2)) != NOTOK
	    && (fd2 = dup (fileno (stderr))) != NOTOK) {
	(void) dup2 (fd3, fileno (stderr));
	(void) close (fd3);
    }
    else
	fd2 = NOTOK;
    for (ap = brkstring (cp = getcpy (cp), " ", NULLCP); *ap; ap++)
	(void) m_convert (mp, *ap);
    free (cp);
    if (fd2 != NOTOK)
	(void) dup2 (fd2, fileno (stderr));
    if (mp -> numsel == 0) {
	if (debugsw)
	    admonish (NULLCP, "no messages to annotate");
	goto oops;
    }

    (void) lseek (fd, 0L, 0);
    if ((fp = fdopen (fd, "r")) == NULL) {
	if (debugsw)
	    admonish (NULLCP, "unable to fdopen annotation list");
	goto oops;
    }
    cp = NULL;
    while (fgets (buffer, sizeof buffer, fp) != NULL)
	cp = add (buffer, cp);
    (void) fclose (fp);

    if (debugsw)
	advise (NULLCP, "annotate%s with %s: \"%s\"",
		inplace ? " inplace" : "", annotext, cp);
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if (debugsw)
		advise (NULLCP, "annotate message %d", msgnum);
	    (void) annotate (m_name (msgnum), annotext, cp, inplace);
	}

    free (cp);

oops: ;
    m_fmsg (mp);
}

/*  */

void done (status)
int	status;
{
    if (armed)
	longjmp (env, status ? status : NOTOK);

    exit (status);
}
