/* umhook.c - one attempt at a rcvmail hook for UUCP mail */
#ifndef	lint
static char ident[] = "@(#)$Id: umhook.c,v 1.4 1993/08/25 17:29:36 jromine Exp $";
#endif	lint

/* I don't comment my code heavily, so read this...

    You run this program from your .login file.  The invocation is simply
    "umhook".  The program "detaches" itself and runs unattended until you
    logout.  Whenever you get UUCP mail (or upto a minute afterwards),
    umhook will filter your UUCP mail drop to a temporary file.  The mail
    drop is *NOT* touched beyond this (even the access time remains the
    same).  For each message that was new in the mail drop, umhook will
    fork a process to interpret your .maildelivery file.

    The umhook program uses the -ljobs control facility to do two things:
	- determine when the controlling tty has gone away
	- kill a child that's run away (the child sets up a process group)
 */

#include "../h/mh.h"
#include "../zotnet/mf.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <pwd.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	SLEEPSW	0
    "sleep seconds", 0,

#define	HELPSW	1
    "help", 4,

    NULL, NULL
};

/*  */

static int  snooze = 60;

static int  uucp = NOTOK;

extern char *environ;

static char myhome[BUFSIZ] = "";
static char mymail[BUFSIZ] = "";
static char myaddr[BUFSIZ] = "";
static char mystat[BUFSIZ] = "";
static char myuser[BUFSIZ] = "";

int	sigser ();

off_t    lseek ();
#ifdef	SYS5
#ifndef __STDC__
struct passwd  *getpwuid ();
#endif /* !__STDC__ */
#endif	SYS5

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    char   *cp,
          **ap,
          **argp,
            buf[100],
           *arguments[MAXARGS];
    struct passwd  *pw;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

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
		    (void) sprintf (buf, "%s [switches]", invo_name);
		    help (buf, switches);
		    done (1);

		case SLEEPSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((snooze = atoi (cp)) < 0)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;
	    }
	adios (NULLCP, "usage: %s [switches]", invo_name);
    }

/*  */

    if ((pw = getpwuid (getuid ())) == NULL)
	adios (NULLCP, "you lose big");

    *environ = NULL;
    (void) m_putenv ("USER", pw -> pw_name);
    (void) m_putenv ("HOME", pw -> pw_dir);
    (void) m_putenv ("SHELL", pw -> pw_shell);
    if (chdir (pw -> pw_dir) == NOTOK)
	(void) chdir ("/");
    (void) umask (0077);

    if (geteuid () == 0) {
#ifdef	BSD41A
	(void) inigrp (pw -> pw_name, pw -> pw_gid);
#endif	BSD41A
	(void) setgid (pw -> pw_gid);
#ifdef	BSD42
	(void) initgroups (pw -> pw_name, pw -> pw_gid);
#endif	BSD42
	(void) setuid (pw -> pw_uid);
    }

    (void) sprintf (mymail, "%s/%s",
	    uucpldir[0] ? uucpldir : pw -> pw_dir,
	    uucplfil[0] ? uucplfil : pw -> pw_name);
    (void) strcpy (myuser, pw -> pw_name);
    (void) sprintf (myaddr, "%s@%s", pw -> pw_name, LocalName ());
    (void) strcpy (myhome, pw -> pw_dir);
    (void) sprintf (mystat, ".%s_%d", invo_name, pw -> pw_uid);

    if (access (slocalproc, 1) == NOTOK)
	adios (slocalproc, "unable to execute");

    closefds (fileno (stderr) + 1);

    (void) signal (SIGINT, SIG_IGN);
    (void) signal (SIGHUP, sigser);
    (void) signal (SIGQUIT, SIG_IGN);
    (void) signal (SIGTERM, sigser);

    switch (fork ()) {
	case NOTOK: 
	case OK: 
	    umhook ();
	    break;

	default: 
	    break;
    }

    exit (0);
}

/*  */

#ifndef	TIOCGPGRP
#define	pgrp_ok(pg)	1
#else	TIOCGPGRP
#define	pgrp_ok(pg)	(ioctl (2, TIOCGPGRP, (char *) &pg) != NOTOK)
#endif	TIOCGPGRP

static  umhook () {
    int     pg;
    struct stat st1,
                st2;

    st_init (&st1);

    for (; pgrp_ok (pg);) {
	if (stat (mymail, &st2) == NOTOK) {
	    st2.st_ino = (ino_t) 0;
	    st2.st_size = (off_t) 0;
	    st2.st_mtime = (time_t) 0;
	}
	else
	    if (st1.st_mtime != st2.st_mtime)
		if (st1.st_ino != st2.st_ino)
		    process ((off_t) 0, &st2);
		else
		    if (st1.st_size < st2.st_size)
			process (st1.st_size, &st2);

	st1.st_ino = st2.st_ino;
	st1.st_size = st2.st_size;
	st1.st_mtime = st2.st_mtime;

	sleep ((unsigned) snooze);
    }
}

/*  */

static  process (offset, st)
off_t offset;
struct stat *st;
{
    int	    td1,
            td2;
    time_t timep[2];
    char    tmpfil[BUFSIZ];
    register FILE *fp;

    if ((uucp = lkopen (mymail, 0)) == NOTOK)
	adios (NULLCP, "unable to lock and open %s", mymail);
    if (lseek (uucp, (off_t) offset, 0) == (off_t) NOTOK)
	adios (mymail, "unable to position to %ld offset on", (long) offset);

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((td1 = creat (tmpfil, TMPMODE)) == NOTOK)
	adios (tmpfil, "unable to create");
    (void) close (td1);

    if ((td1 = open (tmpfil, 2)) == NOTOK)
	adios (tmpfil, "unable to open");
    (void) unlink (tmpfil);
    if ((td2 = dup (td1)) == NOTOK)
	adios ("file descriptor", "unable to dup");

    switch (uucp2mmdf (uucp, td1, FALSE)) {
	case MFPRM: 
	    adios (NULLCP, "internal error while filtering UUCP mail");

	case MFSIO: 
	    adios (NULLCP, "no free file pointers");

	case MFERR: 
	    adios ("UUCP mail", "i/o error while filtering");

	case MFOK: 
	case MFROM: 
	case MFHDR: 
	case MFTXT: 
	    timep[0] = st -> st_atime;
	    timep[1] = st -> st_mtime;
	    utime (mymail, timep);
	    st_update (st);
	    break;
    }
    (void) lkclose (uucp, mymail), uucp = NOTOK;

/*  */

    (void) close (td1);

    (void) lseek (td2, (off_t)0, 0);
    if ((fp = fdopen (td2, "r")) == NULL)
	adios (NULLCP, "no free file pointers");

    while (hook (fp))
	continue;
    (void) fclose (fp);
}

/*  */

static int  hook (in)
register FILE *in;
{
    int     child_id,
            done,
            fd1,
            fd2,
            i;
    char    buffer[BUFSIZ],
            mysndr[BUFSIZ],
            myfile[BUFSIZ];
    register FILE *out;

    if (fgets (buffer, sizeof buffer, in) == NULL)
	return FALSE;

/* should insist on isdlm1 (buffer) here... */

    (void) strcpy (myfile, m_tmpfil (invo_name));
    if ((fd1 = creat (myfile, TMPMODE)) == NOTOK)
	adios (myfile, "unable to create");
    (void) close (fd1);

    if ((fd1 = open (myfile, 2)) == NOTOK)
	adios (myfile, "unable to open");
    (void) unlink (myfile);
    if ((fd2 = dup (fd1)) == NOTOK)
	adios ("file descriptor", "unable to dup");

    if ((out = fdopen (fd1, "w")) == NULL)
	adios (NULLCP, "no free file pointers");

    for (done = TRUE;;) {
	if (fgets (buffer, sizeof buffer, in) == NULL)
	    break;		/* should be error */
	if (done && isdlm2 (buffer))
	    break;
	done = buffer[strlen (buffer) - 1] == '\n';
	fputs (buffer, out);
    }
    (void) fclose (out);

    (void) lseek (fd2, (off_t)0, 0);
    seeksndr (fd2, mysndr);

/*  */

    switch (child_id = fork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");/* NOTREACHED */

	case OK: 
	    (void) lseek (fd2, (off_t)0, 0);
	    if (fd2 != 0)
		(void) dup2 (fd2, 0);
	    (void) freopen ("/dev/null", "w", stdout);
	    (void) freopen ("/dev/null", "w", stderr);
	    if (fd2 != 3)
		(void) dup2 (fd2, 3);
	    closefds (4);
#ifdef	TIOCNOTTY
	    if ((i = open ("/dev/tty", 2)) != NOTOK) {
		(void) ioctl (i, TIOCNOTTY, NULLCP);
		(void) close (i);
	    }
#endif	TIOCNOTTY
#ifdef	BSD42
	    (void) setpgrp (0, getpid ());
#endif	BSD42

	    execlp (slocalproc, r1bindex (slocalproc, '/'),
		    "-file", myfile, "-mailbox", mymail,
		    "-home", myhome, "-addr", myaddr,
		    "-user", myuser, "-sender", mysndr, NULLCP);
	    adios (slocalproc, "unable to exec");/* NOTREACHED */

	default: 
	    (void) close (fd2);
	    (void) pidwait (child_id, OK);
	    return TRUE;
    }
}

/*  */

static  seeksndr (fd1, mysndr)
int     fd1;
char   *mysndr;
{
    int     fd2;
    char   *bp,
           *hp,
            from[BUFSIZ],
            sender[BUFSIZ];
    register FILE *in;

    if ((fd2 = dup (fd1)) == NOTOK)
	adios ("file descriptor", "unable to dup");
    if ((in = fdopen (fd2, "r")) == NULL)
	adios (NULLCP, "no free file pointers");

    for (from[0] = sender[0] = NULL; mfgets (in, &hp) != DONE;)
	if ((bp = index (hp, ':')) != NULL) {
	    *bp++ = NULL;
	    if (lequal (hp, "From"))
		seekaddr (from, bp);
	    else
		if (lequal (hp, "Sender"))
		    seekaddr (sender, bp);
	}
    (void) fclose (in);

    (void) strcpy (mysndr, sender[0] ? sender : from[0] ? from : myaddr);
}

/*  */

static  seekaddr (addr, bp)
char   *addr,
       *bp;
{
    struct adrx *adrxp;

    if ((adrxp = seekadrx (bp)) == NULL)
	return;
    if (adrxp -> err || !adrxp -> mbox)
	return;

    if (adrxp -> host)
	(void) sprintf (addr, "%s@%s", adrxp -> mbox, adrxp -> host);
    else
	(void) strcpy (addr, adrxp -> mbox);

    while (seekadrx (NULLCP))
	continue;
}

/*  */

static st_init(st)
struct stat *st;
{
    int     fd;

    if ((fd = open (mystat, 0)) == NOTOK
	    || read (fd, (char *) st, sizeof *st) != (sizeof *st)) {
	st -> st_ino = (ino_t) 0;
	st -> st_size = (off_t) 0;
	st -> st_mtime = (time_t) 0;
    }
    if (fd != NOTOK)
	(void) close (fd);
}


static st_update(st)
struct stat *st;
{
    static int  fd = NOTOK;

    if (fd == NOTOK
	    && (fd = creat (mystat, TMPMODE)) == NOTOK)
	adios (mystat, "unable to write");

    (void) lseek (fd, (off_t)0, 0);
    if (write (fd, (char *) st, sizeof *st) != (sizeof *st))
	adios (mystat, "error writing");
}

/*  */

#ifdef	BSD42
/* ARGSUSED */
#endif	BSD42

static int  sigser (sig)
int     sig;
{
#ifndef	BSD42
    (void) signal (sig, SIG_IGN);
#endif	BSD42

    done (1);
}

/*  */

void	done (status)
int	status;
{
    (void) lkclose (uucp, mymail), uucp = NOTOK;
    exit (status);
}
