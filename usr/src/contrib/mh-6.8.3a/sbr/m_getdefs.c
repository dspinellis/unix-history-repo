/* m_getdefs.c - read the user's MH environment */
#ifndef	lint
static char ident[] = "@(#)$Id: m_getdefs.c,v 1.8 1993/09/03 17:38:51 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>


extern int errno;

#ifndef __STDC__
#ifdef	SYS5
struct	passwd	*getpwuid ();
#endif
#endif /* !__STDC__ */


#ifndef	OVERHEAD
#define	FOpen(f,m,e,c)	fopen (f, m)
#define	FClose(f)	fclose (f)
#else	/* OVERHEAD */
static FILE    *FOpen ();
static int	FClose ();

off_t	lseek ();
#endif	/* OVERHEAD */


void m_getdefs() {
    int     pid;
    register char  *cp,
                   *pp;
#if	defined (notdef) || defined (COMPAT)
    register char  *ctx;
#endif	/* notdef or COMPAT */
    char    buf[BUFSIZ];
    struct stat st;
    register struct passwd *pw;
    register    FILE *ib;

    if (defpath)
	return;
    if (mypath == NULL) {
	if (mypath = getenv ("HOME"))
	    mypath = getcpy (mypath);
	else
	    if ((pw = getpwuid (getuid ())) == NULL
		    || pw -> pw_dir == NULL
		    || *pw -> pw_dir == 0)
		adios (NULLCP, "no HOME envariable");
	    else
		mypath = getcpy (pw -> pw_dir);
	if ((cp = mypath + strlen (mypath) - 1) > mypath && *cp == '/')
	    *cp = 0;
    }

    if ((cp = getenv ("MH")) && *cp) {
	defpath = path (cp, TFILE);
	if ((ib = FOpen (defpath, "r", "MHFD", 0)) == NULL)
	    adios (defpath, "unable to read");
	if (*cp != '/')
	    (void) m_putenv ("MH", defpath);
    }
    else {
	defpath = concat (mypath, "/", mh_profile, NULLCP);

	if ((ib = FOpen (defpath, "r", "MHFD", 0)) == NULL)
	    switch (pid = vfork ()) {
		case NOTOK:
		    adios ("fork", "unable to");

		case OK:
		    (void) setgid (getgid ());
		    (void) setuid (getuid ());

		    execlp (installproc, "install-mh", "-auto", NULLCP);
		    fprintf (stderr, "unable to exec ");
		    perror (installproc);
		    _exit (-1);

		default:
		    if (pidwait (pid, OK)
			    || (ib = fopen (defpath, "r")) == NULL)
			adios (NULLCP, "[install-mh aborted]");
	    }
    }

#if	defined (notdef) || defined (COMPAT)
    ctx = context;
#endif	/* notdef or COMPAT */
    m_readefs (&m_defs, ib, mh_profile, 0);
    (void) FClose (ib);

    if ((pp = m_find ("path")) != NULL && *pp) {
	if (*pp != '/')
	    (void) sprintf (buf, "%s/%s", mypath, pp);
	else
	    (void) strcpy (buf, pp);
	if (stat(buf, &st) == NOTOK) {
	    if (errno != ENOENT)
		adios (buf, "error opening");
	    cp = concat ("Your MH-directory \"", buf,
		"\" doesn't exist; Create it? ", NULLCP);
	    if (!getanswer(cp))
		adios (NULLCP, "unable to access MH-directory \"%s\"", buf);
	    free (cp);
	    if (!makedir (buf))
		adios (NULLCP, "unable to create", buf);
	}
    }

#ifdef	COMPAT
    if (strcmp (ctx, "/dev/null") == 0)
	return;			/* called by install-mh */

    if (access (ctxpath = getcpy (m_maildir (ctx)), 0) == NOTOK)
	switch (pid = fork ()) {
	    case NOTOK:
		adios ("fork", "unable to");

	    case OK:
		(void) setgid (getgid ());
		(void) setuid (getuid ());
		fprintf (stderr, "install-mh -compat\n");

		execlp (installproc, "install-mh", "-compat", NULLCP);
		fprintf (stderr, "unable to exec ");
		perror (installproc);
		_exit (-1);

	    default:
		if (pidwait (pid, OK) || access (ctxpath, 0) == NOTOK)
		    adios (NULLCP, "[install-mh aborted]");

		if ((ib = fopen (defpath, "r")) == NULL)
		    adios (defpath, "unable to read");
		m_readefs (&m_defs, ib, mh_profile, 0);
		(void) FClose (ib);
	}
    free (ctxpath);
#endif	/* COMPAT */

    if ((cp = getenv ("MHCONTEXT")) == NULL || *cp == 0)
	cp = context;
    if ((ib = FOpen (ctxpath = getcpy (m_maildir (cp)), "r", "MHCONTEXTFD", 1))
		== NULL) {
#ifdef	notdef			/* XXX */
	if (cp != ctx)
	    adios (ctxpath, "unable to read");
#endif	/* notdef */
    }
    else {
	m_readefs ((struct node **) 0, ib, cp, 1);
	(void) FClose (ib);
    }
}

/*  */

#ifdef	OVERHEAD
int	fd_def = NOTOK;
int	fd_ctx = NOTOK;


static FILE *FOpen (filename, mode, envariable, ctx)
register char   *filename,
                *mode,
	        *envariable;
register int     ctx;
{
    register int    fd;
    register char  *cp;
    char    buffer[10];
    struct stat st1;
    register    FILE * fp;

    if ((cp = getenv (envariable)) == NULL || *cp == 0)
	goto not_open;

    if ((fd = atoi (cp)) <= fileno (stderr)) {
	advise (NULLCP, "bad value for $%s: %s", envariable, cp);
	(void) unputenv (envariable);
	return fopen (filename, mode);
    }
    if (ctx)
	fd_ctx = fd;
    else
	fd_def = fd;

    if (fstat (fd, &st1) == NOTOK) {
not_open: ;
	if ((fp = fopen (filename, mode))
		&& !strcmp (mode, "r")
		&& fileno (fp) > fileno (stderr)) {
	    (void) sprintf (buffer, "%d", fileno (fp));
	    if (!m_putenv (envariable, buffer))
		if (ctx)
		    fd_ctx = fileno (fp);
		else
		    fd_def = fileno (fp);
	}

	if ((cp = getenv ("MHFDEBUG")) && *cp)
	    fprintf (stderr, "FOpen of %s sets %d\n", filename, fileno (fp));
	return fp;
    }

    (void) lseek (fd, (off_t)0, 0);
    return fdopen (fd, mode);
}


static int  FClose (f)
register FILE   *f;
{
    register int    d1,
                    d2,
                    i;
    register char  *cp;

    if (f == NULL)
	return OK;

    if ((d1 = fileno (f)) != fd_def && d1 != fd_ctx)
	return fclose (f);

    d2 = dup (d1);
    i = fclose (f);

    if (d2 != NOTOK) {
	(void) dup2 (d2, d1);
	(void) close (d2);
    }
    else
	if (d1 == fd_def)
	    fd_def = NOTOK;
	else
	    fd_ctx = NOTOK;

    if ((cp = getenv ("MHFDEBUG")) && *cp)
	fprintf (stderr, "FClose emulating close of %d (%d)\n", d1, d2);
    return i;
}
#endif	/* OVERHEAD */
