/* rmail.c - replacement for /bin/rmail */
#ifndef	lint
static char ident[] = "@(#)$Id: rmail.c,v 1.3 1993/08/25 17:27:43 jromine Exp $";
#endif	lint

/* This program has a long, long history.  It started as UCB's rmail, and
   was then modified by OBrien@Rand-Unix to run with MMDF.  Then DpK@Brl
   re-wrote it, and SmB@Unc hacked it up a bit.  After that
   MRose.UCI@Rand-Relay upgraded it to use the nifty MF (mail filtering)
   system.  Finally, the latter stripped it down to work with MH.

   This program should be used ONLY if you have both "mts mh" and "uucp on"
   set in your MH configuration.
 */


#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../zotnet/mf.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <signal.h>
#ifdef LOCALE
#include	<locale.h>
#endif


#define	ADDROK	0		/* okay to use post to deliver message */
#define	UUCP	1		/* okay to use uux to deliver message */
#define	RETURN	2		/* message loses */

/*  */
int     pbroke;			/* broken-pipe flag */
int     rtnflag;		/* note was sent back */

char    date[BUFSIZ];		/* date of origination from uucp header */
char    from[BUFSIZ];		/* accumulated path of sender */
char    origsys[BUFSIZ];	/* originating system */
char    origpath[BUFSIZ];	/* path from us to originating system */
char    usrfrm[BUFSIZ];		/* the 822 version of from[] */
char    Mailsys[BUFSIZ];	/* address of the mail agent */
char    overseer[BUFSIZ];	/* address of the watchdog */

char    mmdf[BUFSIZ];		/* filtered mail file */

char   *rtnmessage[] = {
    "	Your message has been intercepted trying to access\n",
    "a restricted access host (e.g. an ARPANET host).  A copy\n",
    "of your message has been sent to the system administrators.\n",
    "The text of your message follows.\n\n",
    NULL
};

char    rtnbegin[] =
        " ---------------- Returned Mail Follows --------------\n";
char    rtnend[] =
        "  --------------- End of Returned Mail ---------------\n";

char   *oopsmessage[] = {
    "\n\n\tThe system administrators (%s) have been informed of\n",
    "the problem, but have not been given a copy of your message.\n\n",
    NULL
};

FILE * fromf;			/* UUCP "From lines */
FILE * msgf;			/* message text */
FILE * pipef;			/* output for "post" or "uux" */


int	pipeser ();


off_t    lseek ();

/*  */

main (argc, argv)
int     argc;
char  **argv;
{
    int     cpyback;
    char   *cp,
           *fromptr,
            fromwhom[BUFSIZ],
            linebuf[BUFSIZ],
            sys[BUFSIZ];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (*argv, '/');
    m_foil (NULLCP);
    mts_init (invo_name);

    if (argc < 2)
	adios (NULLCP, "usage: %s user [user ...]", invo_name);
    (void) umask (0);
    (void) setgid (1);
    (void) setuid (1);

    (void) sprintf (Mailsys, "%s@%s", Mailer, LocalName ());
    if (Overseer == NULL)
	Overseer = Mailsys;
    if (index (Overseer, '@') == NULL) {
	(void) sprintf (overseer, "%s@%s", Overseer, LocalName ());
	Overseer = overseer;
    }

    (void) mktemp (Errtmp);
    if (freopen (Errtmp, "w", stderr) == NULL)
	adios (Errtmp, "unable to create");
    (void) dup2 (fileno (stderr), fileno (stdout));

    (void) mktemp (Msgtmp);
    if ((msgf = fdopen (creat (Msgtmp, Tmpmode), "w")) == NULL)
	adios (Msgtmp, "unable to create");

    (void) mktemp (Fromtmp);
    if ((fromf = fdopen (creat (Fromtmp, Tmpmode), "w")) == NULL)
	adios (Fromtmp, "unable to create");

/*  */

    for (;;) {
	if (fgets (linebuf, sizeof linebuf, stdin) == NULL)
	    break;
	if (strncmp (linebuf, "From ", 5)
		&& strncmp (linebuf, ">From ", 6))
	    break;

	if (linebuf[0] != '>')
	    fputs (">", fromf);
	fputs (linebuf, fromf);
	cp = index (linebuf, ' ');
	fromptr = ++cp;
	cp = index (cp, ' ');
	*cp++ = NULL;
	(void) strcpy (fromwhom, fromptr);
	(void) strncpy (date, cp, 24);

	for (;;) {
	    if ((cp = index (cp + 1, 'r')) == NULL) {
		if ((cp = rindex (fromwhom, '!')) != NULL) {
		    char   *p;
		    *cp = NULL;
		    if ((p = rindex (fromwhom, '!')) != NULL)
			(void) strcpy (origsys, p + 1);
		    else
			(void) strcpy (origsys, fromwhom);
		    (void) strcat (from, fromwhom);
		    (void) strcat (from, "!");
		    (void) strcpy (fromwhom, cp + 1);
		    goto out;
		}
		(void) strcpy (sys, SystemName ());
		(void) strcat (from, sys);
		(void) strcpy (origsys, sys);
		(void) strcat (from, "!");
		goto out;
	    }
	    if (strncmp (cp, "remote from ", 12) == 0)
		break;
	}

	(void) sscanf (cp, "remote from %s", sys);
	(void) strcat (from, sys);
	(void) strcpy (origsys, sys);
	(void) strcat (from, "!");
out: 	;
    }
    if (fromwhom[0] == NULL)
	adios (NULLCP, "no from line");

/*  */

    (void) strcpy (origpath, from);
    (void) strcat (from, fromwhom);
    get_mmdf_addr (from, usrfrm);
    if ((cp = rindex (usrfrm, '<')) != NULL) {
	(void) strcpy (usrfrm, ++cp);	/* sigh */
	if ((cp = rindex (usrfrm, '>')) != NULL)
	    *cp = NULL;
    }
    if (usrfrm[0] == NULL)
	(void) sprintf (usrfrm, "%s!%s%%%s@%s%c",
		SystemName (), from, UucpChan (), LocalName (), NULL);

    fputs (linebuf, msgf);
    if (txtcpy (stdin, msgf) == NOTOK)
	fputs ("\n  *** Problem during receipt from UUCP ***\n", msgf);

    (void) freopen (Msgtmp, "r", msgf);
    (void) freopen (Fromtmp, "r", fromf);
    (void) unlink (Fromtmp);
    mmdf[0] = NULL;

    cpyback = 0;
    for (argv++; --argc > 0;) {
	rewind (fromf);
	rewind (msgf);
	rtnflag = 0;
	if (deliver (*argv++) == NOTOK && !rtnflag)
	    cpyback++;
    }

    (void) fflush (stderr);
    (void) fflush (stdout);

    if (cpyback) {
	rcpy ();
	zcpy ();
    }

    (void) unlink (Msgtmp);
    if (mmdf[0])
	(void) unlink (mmdf);
    (void) unlink (Errtmp);

    exit (0);
}

/*  */

deliver (to)
char   *to;
{
    int     i,
            replyval;
    char    tmpfil[BUFSIZ];

    switch (adrcheck (to)) {
	case ADDROK: 
	    if (mmdf[0] == NULL && filter () == NOTOK)
		(void) strcpy (mmdf, Msgtmp);
	    replyval = xpost (to, mmdf);
	    break;

	case UUCP: 
	    if ((replyval = xuucp (to)) == NOTOK)
		break;

	    if ((replyval = txtcpy (fromf, pipef)) != NOTOK)
		replyval = txtcpy (msgf, pipef);
	    i = (pclose (pipef) >> 8) & 0xff;
	    if (replyval != NOTOK)
		replyval = (i != 0 ? NOTOK : OK);
	    break;

/*  */

	case RETURN: 
	    rtnflag++;
	    switch (adrcheck (from)) {
		case ADDROK: 
		case RETURN: 
		    (void) strcpy (tmpfil, "/tmp/rmailXXXXXX");
		    (void) unlink (mktemp (tmpfil));
		    if ((pipef = fdopen (creat (tmpfil, Tmpmode), "w")) == NULL)
			return NOTOK;

		    fprintf (pipef, "Date: %s\nFrom: %s\n",
			    dtimenow (), Mailsys);
		    fprintf (pipef, "To: %s\ncc: %s\n", from, Overseer);
		    rtnmesg (to);
		    (void) fclose (pipef);

		    replyval = xpost (from, tmpfil);
		    (void) unlink (tmpfil);
		    break;

		case UUCP: 
		    if ((replyval = xuucp (from)) == NOTOK)
			break;

		    fprintf (pipef, "To: %s\ncc: %s\n", from, Overseer);
		    rtnmesg (to);
		    i = (pclose (pipef) >> 8) & 0xff;
		    if (replyval != NOTOK)
			replyval = (i != 0 ? NOTOK : OK);
		    break;
	    }
	    if (Syscpy) {
		(void) strcpy (tmpfil, "/tmp/rmailXXXXXX");
		(void) unlink (mktemp (tmpfil));
		if ((pipef = fdopen (creat (tmpfil, Tmpmode), "w")) == NULL)
		    return NOTOK;

		fprintf (pipef, "Date: %s\nFrom: %s\n",
			dtimenow (), Mailsys);
		fprintf (pipef, "To: %s\ncc: %s\n", usrfrm, Overseer);
		rtnmesg (to);
		(void) fclose (pipef);

		replyval = xpost (Overseer, tmpfil);
		(void) unlink (tmpfil);
	    }
	    break;
    }

    return replyval;
}

/*  */

adrcheck (adr)
char   *adr;
{
    int     type;
    char   *cp,
            host[BUFSIZ];
    struct mailname *mp;

    if ((cp = getname (adr)) == NULL)
	return RETURN;
    mp = getm (cp, NULLCP, 0, AD_HOST, NULLCP);
    while (getname (""))
	continue;
    if (mp == NULL)
	return RETURN;

    type = mp -> m_type;
    (void) strcpy (host, mp -> m_host);
    mnfree (mp);
    if (mp -> m_mbox == NULL)
	return RETURN;

    switch (type) {
	case LOCALHOST: 
	    return ADDROK;

	case UUCPHOST: 
	    return (strcmp (host, SystemName ()) ? UUCP : ADDROK);

	default: 
	    if (lookup (origsys, Okhosts) == OK)
		return ADDROK;
	    return (okhost (host) == NOTOK ? RETURN : ADDROK);
    }
}

/*  */

okhost (host)
char   *host;
{
    return (lookup (origsys, Okhosts) == OK
	    || lookup (host, Okhosts) == OK
	    || lookup (host, Okdests) == OK ? OK : NOTOK);
}


lookup (what, where)
char   *what,
       *where;
{
    char   *cp,
            entry[BUFSIZ];
    FILE * lookf;

    if ((lookf = fopen (where, "r")) == NULL)
	return NOTOK;
    while (fgets (entry, sizeof entry, lookf) != NULL) {
	cp = entry;
	while (*cp != '\n' && *cp != ' ' && *cp != '\t')
	    cp++;
	*cp = NULL;
	if (uleq (what, entry)) {
	    (void) fclose (lookf);
	    return OK;
	}
    }
    (void) fclose (lookf);

    return NOTOK;
}


/*  */

rtnmesg (badadr)
char   *badadr;
{
    int     i;

    fprintf (pipef, "Subject: Illegal Address (%s)\n\n", badadr);
    for (i = 0; rtnmessage[i]; i++)
	fputs (rtnmessage[i], pipef);
    fputs (rtnbegin, pipef);

    rewind (fromf);
    (void) txtcpy (fromf, pipef);
    rewind (msgf);
    (void) txtcpy (msgf, pipef);

    fputs (rtnend, pipef);
}


txtcpy (frm, to)
FILE * frm, *to;
{
    int     nread;
    char    buffer[BUFSIZ];

    while (!pbroke
	    && (nread = fread (buffer, sizeof (*buffer), BUFSIZ, frm)) > 0)
	(void) fwrite (buffer, sizeof (*buffer), nread, to);

    return (ferror (frm) ? NOTOK : OK);
}

/*  */

xpost (addr, file)
char   *addr,
       *file;
{
    int     i,
            child_id;

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	    execlp (postproc, r1bindex (postproc, '/'),
		    "-deliver", addr, file, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (postproc);
	    _exit (1);

	default: 
	    return (pidwait (child_id, OK) ? NOTOK : OK);
    }
}

/*  */

xuucp (to)
char   *to;
{
    char   *cp,
            buffer[BUFSIZ],
            cmdstr[BUFSIZ];

    (void) strcpy (buffer, to);
    if (cp = index (buffer, '!'))
	*cp++ = NULL;
    else {
	fprintf (stderr, "internal error -- %s has no host\n", to);
	return NOTOK;
    }
    (void) sprintf (cmdstr, "uux -p %s!rmail \\(%s\\)", buffer, cp);

    if ((pipef = popen (cmdstr, "w")) == NULL)
	return NOTOK;

    (void) signal (SIGPIPE, pipeser);
    pbroke = 0;

    return OK;
}

/*  */

#ifdef	BSD42
/* ARGSUSED */
#endif	BSD42

static int  pipeser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (i, SIG_IGN);
#endif	BSD42

    pbroke = 1;
}

/*  */

rcpy () {
    int     i;
    char    buffer[BUFSIZ],
            tmpfil[BUFSIZ];
    FILE * fp;

    (void) strcpy (tmpfil, "/tmp/rmailXXXXXX");
    (void) unlink (mktemp (tmpfil));
    if ((pipef = fdopen (creat (tmpfil, Tmpmode), "w")) == NULL)
	return;

    fprintf (pipef, "Date: %s\nFrom: %s\n", dtimenow (), Mailsys);
    fprintf (pipef, "To: %s\n", usrfrm);
    fprintf (pipef, "\nProblems sending mail:\n\n");
    i = 0;
    if ((fp = fopen (Errtmp, "r")) != NULL) {
	while (fgets (buffer, sizeof buffer, fp) != NULL) {
	    if (ferror (pipef))
		break;
	    fputs (buffer, pipef);
	    i++;
	}
    }
    if (i == 0)
	fprintf (pipef, "\tunknown problem\n");
    for (i = 0; oopsmessage[i]; i++)
	fprintf (pipef, oopsmessage[i], Overseer);
    fputs (rtnbegin, pipef);

    rewind (fromf);
    (void) txtcpy (fromf, pipef);
    rewind (msgf);
    (void) txtcpy (msgf, pipef);

    fputs (rtnend, pipef);
    (void) fclose (pipef);

    (void) xpost (usrfrm, tmpfil);
    (void) unlink (tmpfil);
}

/*  */

zcpy () {
    int     i;
    char    buffer[BUFSIZ],
            tmpfil[BUFSIZ];
    FILE * fp;

    (void) strcpy (tmpfil, "/tmp/rmailXXXXXX");
    (void) unlink (mktemp (tmpfil));
    if ((pipef = fdopen (creat (tmpfil, Tmpmode), "w")) == NULL)
	return;

    fprintf (pipef, "Date: %s\nFrom: %s\n", dtimenow (), Mailsys);
    fprintf (pipef, "To: %s\n", Mailsys);
    fprintf (pipef, "\nProblems sending mail for %s (aka %s):\n\n",
	    from, usrfrm);

    i = 0;
    if ((fp = fopen (Errtmp, "r")) != NULL) {
	while (fgets (buffer, sizeof buffer, fp) != NULL) {
	    if (ferror (pipef))
		break;
	    fputs (buffer, pipef);
	    i = 1;
	}
	(void) fclose (fp);
	if (i == 0)
	    fprintf (pipef, "\tunknown problem\n");
    }
    else
	fprintf (pipef, "\tunable to open %s\n", Errtmp);
    (void) fclose (pipef);

    (void) xpost (Mailsys, tmpfil);
    (void) unlink (tmpfil);
}

/*  */

filter () {
    int     i,
            fd,
            td;
    char    tmpfil[BUFSIZ],
            mmdfil[BUFSIZ];
    FILE * out;

    (void) strcpy (tmpfil, "/tmp/rmailXXXXXX");
    (void) unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, Tmpmode)) == NOTOK)
	return NOTOK;
    (void) close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK)
	return NOTOK;
    if ((out = fdopen (fd, "w")) == NULL) {
	(void) close (fd);
	return NOTOK;
    }
    if ((td = dup (fd)) == NOTOK) {
	(void) close (fd);
	return NOTOK;
    }

    fprintf (out, "From %s %s\n", from, date);
    if (txtcpy (msgf, out) == NOTOK) {
	(void) close (fd);
	(void) close (td);
	return NOTOK;
    }
    (void) fclose (out);
    (void) lseek (td, (off_t)0, 0);

    (void) strcpy (mmdfil, "/tmp/mmdfXXXXXX");
    (void) unlink (mktemp (mmdfil));
    if ((fd = creat (mmdfil, Tmpmode)) == NOTOK) {
	(void) close (td);
	(void) unlink (tmpfil);
	return NOTOK;
    }
    if ((fd = open (mmdfil, 2)) == NOTOK) {
	(void) close (td);
	(void) unlink (tmpfil);
	return NOTOK;
    }

/*  */

    switch (i = uucp2mmdf (td, fd, TRUE)) {
	case OK: 
	    (void) strcpy (mmdf, mmdfil);
	    break;

	default: 
	    mmdf[0] = NULL;
	    break;
    }
    (void) close (td);
    (void) unlink (tmpfil);
    (void) close (fd);

    return (i != OK ? NOTOK : OK);
}

/*  */

get_mmdf_addr (addr, to)
char   *addr,
       *to;
{
    struct adrx *adrxp;

    *to = NULL;
    if ((adrxp = seekadrx (addr)) == NULL)
	return;

    addr_convert (adrxp, to, TRUE);
    while (seekadrx (NULLCP))
	continue;
}
