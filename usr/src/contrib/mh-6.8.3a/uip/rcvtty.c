/* rcvtty.c - a rcvmail program (a lot like rcvalert) handling IPC ttys */
#ifndef	lint
static char ident[] = "@(#)$Id: rcvtty.c,v 1.12 1993/08/25 17:27:27 jromine Exp $";
#endif	/* lint */

#ifndef	BSD42
#undef	TTYD
#endif
#include "../h/mh.h"
#include "../h/rcvmail.h"
#include "../h/scansbr.h"
#include "../zotnet/tws.h"
#include <signal.h>
#include <sys/stat.h>
#ifndef	TTYD
#include <utmp.h>
#endif	/* not TTYD */
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */
#define	SCANFMT	\
"%2(hour{dtimenow}):%02(min{dtimenow}): %<(size)%5(size) %>%<{encrypted}E%>\
%<(mymbox{from})%<{to}To:%14(friendly{to})%>%>%<(zero)%17(friendly{from})%>  \
%{subject}%<{body}<<%{body}>>%>"

static struct swit switches[] = {
#define	HELPSW	0
    "help", 4,

#define	BIFFSW	1
    "biff", 0,

#define	FORMSW	2
    "form formatfile", 0,
#define	FMTSW	3
    "format string", 5,

#define NLSW    4
    "newline", 0,
#define NNLSW   5
    "nonewline", 0,
#define BELSW	6
    "bell", 0,
#define	NBELSW	7
    "nobell", 0,

    NULL, 0
};

/*  */

static  jmp_buf myctx;

off_t	lseek ();
char   *getusr ();

static int	message_fd(), header_fd();
static		alert();

static int bell = 1;
static int newline = 1;
static int biff = 0;
static char *form = NULL;
static char *format = NULL;

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   **argv;
{
    int     md,
	    vecp = 0;
    char   *cp,
           *user,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
	   *vec[MAXARGS];
#ifndef	TTYD
    char    tty[BUFSIZ];
    struct utmp ut;
    register FILE  *uf;
#endif	/* not TTYD */

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
		    vec[vecp++] = --cp;
		    continue;
		case HELPSW: 
		    (void) sprintf (buf, "%s [command ...]", invo_name);
		    help (buf, switches);
		    done (1);

		case BIFFSW:
		    biff = 1;
		    continue;

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    format = NULL;
		    continue;
		case FMTSW: 
		    if (!(format = *argp++) || *format == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    form = NULL;
		    continue;

                case NLSW:
                    newline = 1;
                    continue;
                case NNLSW:
                    newline = 0;
                    continue;
                case BELSW:
                    bell = 1;
                    continue;
                case NBELSW:
                    bell = 0;
                    continue;

	    }
	vec[vecp++] = cp;
    }
    vec[vecp] = 0;

/*  */

    if ((md = vecp ? message_fd (vec) : header_fd ()) == NOTOK)
	exit (RCV_MBX);

    user = getusr ();
#ifndef	TTYD
    if ((uf = fopen ("/etc/utmp", "r")) == NULL)
	exit (RCV_MBX);
    while (fread ((char *) &ut, sizeof ut, 1, uf) == 1)
	if (ut.ut_name[0] != 0
		&& strncmp (user, ut.ut_name, sizeof ut.ut_name) == 0) {
	    (void) strncpy (tty, ut.ut_line, sizeof ut.ut_line);
	    alert (tty, md);
	}
    (void) fclose (uf);
#else	/* TTYD */
    alert (user, md);
#endif	/* TTYD */

    exit (RCV_MOK);
}

/*  */

/* ARGSUSED */

static	TYPESIG alrmser (i)
int     i;
{
    longjmp (myctx, DONE);
}


static int  message_fd (vec)
char   *vec[];
{
    int     bytes,
	    child_id,
            fd;
    char    tmpfil[BUFSIZ];
    struct stat st;

    (void) unlink (mktemp (strcpy (tmpfil, "/tmp/rcvttyXXXXX")));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	return header_fd ();
    (void) close (fd);

    if ((fd = open (tmpfil, 2)) == NOTOK)
	return header_fd ();
    (void) unlink (tmpfil);

/*  */

    switch (child_id = vfork ()) {
	case NOTOK: 
	    (void) close (fd);
	    return header_fd ();

	case OK: 
	    rewind (stdin);
	    if (dup2 (fd, 1) == NOTOK || dup2 (fd, 2) == NOTOK)
		_exit (-1);
	    closefds (3);
#ifdef	BSD42
	    (void) setpgrp (0, getpid ());
#endif	/* BSD42 */
	    execvp (vec[0], vec);
	    _exit (-1);

	default: 
	    switch (setjmp (myctx)) {
		case OK: 
		    (void) signal (SIGALRM, alrmser);
		    bytes = fstat (fileno (stdin), &st) != NOTOK
			? (int) st.st_size : 100;
		    if (bytes <= 0)
			bytes = 100;
		    (void) alarm ((unsigned) (bytes * 60 + 300));

		    (void) pidwait (child_id, OK);

		    (void) alarm (0);
		    if (fstat (fd, &st) != NOTOK && st.st_size > (off_t)0)
			return fd;
		    (void) close (fd);
		    return header_fd ();

		default: 
#ifndef	BSD42
		    (void) kill (child_id, SIGKILL);
#else	/* BSD42 */
		    (void) killpg (child_id, SIGKILL);
#endif	/* BSD42 */
		    (void) close (fd);
		    return header_fd ();
	    }
    }
}

/*  */

static int  header_fd () {
    int     fd;
    char    tmpfil[BUFSIZ];

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	return NOTOK;
    (void) close (fd);

    if ((fd = open (tmpfil, 2)) == NOTOK)
	return NOTOK;
    (void) unlink (tmpfil);

    rewind (stdin);
    (void) scan (stdin, 0, 0, new_fs (form, format, SCANFMT), 0, 0, 0, 0, 0L, 0);
    if ( newline )
        (void) write (fd, "\n\r", 2);
    (void) write (fd, scanl, strlen (scanl));
    if ( bell )
        (void) write (fd, "\007", 1);

    return fd;
}

/*  */

#ifndef	TTYD
static  alert (tty, md)
char   *tty;
int     md;
{
    int     i,
            td;
    char    buffer[BUFSIZ],
            ttyspec[BUFSIZ];
    struct stat st;

    (void) sprintf (ttyspec, "/dev/%s", tty);
    if (stat (ttyspec, &st) == NOTOK ||
	(st.st_mode & (biff ? S_IEXEC :
#ifdef	BSD43
		       (S_IWRITE >> 3)
#else	/* BSD43 */
		       02
#endif	/* BSD43 */
		       )) == 0)
	return;

    switch (setjmp (myctx)) {
	case OK: 
	    (void) signal (SIGALRM, alrmser);
	    (void) alarm (2);
	    td = open (ttyspec, 1);
	    (void) alarm (0);
	    if (td == NOTOK)
		return;
	    break;

	default: 
	    (void) alarm (0);
	    return;
    }

    (void) lseek (md, (off_t)0, 0);

    while ((i = read (md, buffer, sizeof buffer)) > 0)
	if (write (td, buffer, i) != i)
	    break;

    (void) close (td);
}
#else	/* TTYD */

/*  */

static  alert (user, md)
register char   *user;
int     md;
{
    int     i,
            td;
    char    buffer[BUFSIZ];

    if ((td = ttyw ("notify", NULLCP, NULLCP, user)) == NOTOK)
	return;
    (void) signal (SIGPIPE, SIG_IGN);

    (void) lseek (md, (off_t), 0);
    while ((i = read (md, buffer, sizeof buffer)) > 0)
	if (write (td, buffer, i) != i)
	    break;

    (void) close (td);
}
#endif	/* TTYD */
