#ifndef	SPOP
/* sbboards.c - MH style mailer to write to a ZOTnet BBoard */
#else	SPOP
/* spop.c - MH style mailer to write to a POP subscriber */
#endif	SPOP
#ifndef	lint
static char ident[] = "@(#)$Id: sbboards.c,v 1.9 1993/08/25 17:27:54 jromine Exp $";
#endif	lint

#ifndef	SPOP

/* This program acts like the MMDF ch_bboards channel: it does local
   delivery to a ZOTnet BBoard and/or addition re-distribution to other
   recipients of the BBoard.  This program can function both as a SendMail
   mailer and an MH .mh_receive file, depending on whether SENDMTS or
   MHMTS is set.  Currently, the MHMTS version of this program does not do
   re-distribution.

   This program should be used ONLY if you have "bboards on" set in your
   MH configuration, and if you have "mts sendmail" or "mts mh" set as well.
 */

#else	SPOP

/* This program acts like the MMDF-II ch_pop channel: it does local
   delivery for non-local users.  These users are known as POP subscribers
   and use the Post Office Protocol with a POP server in order to access
   their maildrop.
 */

#endif	SPOP

#undef	DISTRIBUTE
#ifdef	SENDMTS
#ifndef	SPOP
#define	DISTRIBUTE
#endif	not SPOP
#endif	SENDMTS

#include "../h/mh.h"
#ifndef	SPOP
#include "../h/addrsbr.h"
#endif	not SPOP
#include "../h/dropsbr.h"
#include "../zotnet/bboards.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <pwd.h>
#ifndef	SYS5
#include <sysexits.h>
#else	SYS5
#define	EX_CANTCREAT	1
#define	EX_IOERR	1
#define	EX_NOINPUT	1
#define	EX_NOUSER	1
#define	EX_OK		0
#define	EX_OSERR	1
#define	EX_OSFILE	1
#define	EX_UNAVAILABLE	1
#define	EX_USAGE	1
#endif	SYS5
#ifdef	DISTRIBUTE
#include "../mts/sendmail/smail.h"
#endif	DISTRIBUTE
#ifdef LOCALE
#include	<locale.h>
#endif


#define	NBB	100

#ifndef	SPOP
#define	ENTITY	"bboard"
#else	SPOP
#define	ENTITY	"subscriber"
#endif	SPOP

/*  */

static int  bb_fderr;

static int  bb_uid;
static int  bb_gid;

static int    dst_rcpt ();


#ifndef	SPOP
static char bb_from[BUFSIZ];
static char bb_head[BUFSIZ];
static char bb_home[BUFSIZ];
static char bb_time[BUFSIZ];
#ifdef	DISTRIBUTE
static char bb_rept[BUFSIZ];
#endif	DISTRIBUTE
#else	SPOP
#define	bb_head	NULLCP
#endif	SPOP

static struct bboard  *bb[NBB];


off_t	lseek ();

#ifndef	__STDC__
#ifdef	SYS5
struct passwd  *getpwnam ();
#endif	/* SYS5 */
#endif

#ifndef	SPOP
static int	mbx_init();
#endif
#ifdef	DISTRIBUTE
static int 	distribute(), notify(), encap(),
		dst_init(), dst_text(),
		dst_end(), dst_lose(), dst_adrs();
#endif

/* hack */
#define	adios	my_adios
static	localmail(), arginit();
static int	lose(), copyfile();
static void	adios();
/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int     fd;
    char    tmpfil[BUFSIZ];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
#ifdef	MHMTS
    if (argc != 5)
	adios (EX_USAGE, NULL, "you lose really big");
#endif	MHMTS
    arginit (argv);

    fflush (stdout);
    discard (stdout);		/* XXX: reference discard to help loader */

    fd = copyfile (fileno (stdin), tmpfil);
    (void) unlink (tmpfil);

    localmail (fd);
#ifdef	DISTRIBUTE
    distribute (fd);
    notify (fd);
#endif	DISTRIBUTE

    exit (EX_OK);
}

/*  */

static  localmail (fd)
int     fd;
{
    int     i,
            md;
    register struct bboard  *bp;

    for (i = 0; bp = bb[i]; i++)
	if (bp -> bb_file && *bp -> bb_file) {
	    (void) lseek (fd, (off_t)0, 0);
#ifndef	SPOP
	    if ((md = mbx_open (bp -> bb_file, bb_uid, bb_gid, BBMODE))
#else	SPOP
	    if ((md = mbx_open (bp -> bb_file, bb_uid, bb_gid, POMODE))
#endif	SPOP
		    == NOTOK) {
		(void) lose ("unable to open %s", bp -> bb_file);
		continue;
	    }
#ifndef	SPOP
	    if (mbx_init (bp) != NOTOK)
#endif	not SPOP
		(void) mbx_copy (bp -> bb_file, md, fd, 1, bb_head, 0);
	    (void) mbx_close (bp -> bb_file, md);
	}
}

/*  */

#ifndef	SPOP
static int  mbx_init (bp)
register struct bboard  *bp;
{
    int	    fd,
	    clear;
    register struct bboard  *ip;
    register FILE *fp;

    if ((fd = mbx_Xopen (bp -> bb_info, bb_uid, bb_gid, BBMODE, &clear))
	    == NOTOK)
	return lose ("unable to lock and open %s", bp -> bb_info);
    if ((fp = fdopen (fd, "w")) == NULL) {
        (void) mbx_close (bp -> bb_info, fd);
	return lose ("unable to fdopen %s", bp -> bb_info);
    }

    if ((ip = getbbnam (bp -> bb_name)) == NULL) {
	(void) lkfclose (fp, bp -> bb_info);
	return lose ("unable to get information on BBoard %s", bp -> bb_name);
    }
    (void) strcpy (bb_time, dtimenow ());
    (void) sprintf (bb_head, "BBoard-ID: %d\nBB-Posted: %s\n",
	    bp -> bb_maxima = ++ip -> bb_maxima, bb_time);

    fprintf (fp, "%d\n%s\n", bp -> bb_maxima, bb_time);
    (void) lkfclose (fp, bp -> bb_info);

    return OK;
}
#endif	not SPOP

/*  */

#ifdef	DISTRIBUTE
static  distribute (fd)
int     fd;
{
    int     i;
    register struct bboard  *bp;

    for (i = 0; bp = bb[i]; i++)
	if (bp -> bb_dist && *bp -> bb_dist)
	    break;
    if (bp == NULL)
	return;

    if (dst_init () == NOTOK) {
	dst_lose ();
	return;
    }
    for (i = 0; bp = bb[i]; i++)
	if (bp -> bb_dist && *bp -> bb_dist)
	    if (dst_adrs (bp) == NOTOK) {
		dst_lose ();
		return;
	    }
    if (dst_text (fd) == NOTOK || dst_end () == NOTOK)
	dst_lose ();
}

/*  */

static int  dst_init ()
{
    int     retval;

    if (rp_isbad (retval = sm_init (NULLCP, NULLCP, 0, 0, 0, 0, 0))
	    || rp_isbad (retval = sm_winit (S_MAIL, bb_from)))
	return lose ("problem initializing SendMail; %s",
		rp_string (retval));

    return OK;
}

/*  */

static int  dst_adrs (bp)
register struct bboard  *bp;
{
    if (getbbdist (bp, dst_rcpt))
	return lose ("getbbdist failed: %s", getbberr ());

    return OK;
}

/*  */

static int  dst_rcpt (mbox, host)
register char   *mbox,
		*host;
{
    int     retval;

    switch (retval = sm_wadr (mbox, host, NULLCP)) {
	case RP_OK:
	    return OK;

	case RP_NO:
	case RP_USER:
	    (void) lose ("%s@%s: loses; %s", mbox, host, rp_string (retval));
	    return OK;		/* fail-soft */

	default:
	    return lose ("%s@%s: unexpected response; %s",
		    mbox, host, rp_string (retval));
    }
}

/*  */

static int  dst_text (fd)
int     fd;
{
    int     i,
            retval;
    char    buffer[BUFSIZ];

    if (rp_isbad (retval = sm_waend ()))
	return lose ("problem ending addresses; %s", rp_string (retval));

    (void) lseek (fd, (off_t)0, 0);
    while ((i = read (fd, buffer, sizeof buffer)) > 0)
	if (rp_isbad (retval = sm_wtxt (buffer, i)))
	    return lose ("problem writing text; %s", rp_string (retval));

    return (i != NOTOK ? OK : lose ("error reading from file"));
}

/*  */

static int  dst_end ()
{
    int     retval;

    switch (retval = sm_wtend ()) {
	case RP_OK:
	    (void) sm_end (OK);
	    return OK;

	case RP_NO:
	case RP_NDEL:
	    return lose ("posting failed; %s", rp_string (retval));

	default:
	    return lose ("unexpected response; %s", rp_string (retval));
    }
}

/*  */

static  dst_lose ()
{
    (void) sm_end (NOTOK);
}

/*  */

/* VARARGS1 */

static int  lose (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    int     fd,
            i;
    char   *bp,
	    buffer[BUFSIZ];

    if (bb_fderr == NOTOK) {
	if ((fd = open ("/dev/null", 0)) == NOTOK)
	    adios (EX_OSERR, "/dev/null", "unable to open");
	bb_fderr = copyfile (fd, bb_rept);
    }

    (void) sprintf (bp = buffer, fmt, a, b, c, d);
    bp += strlen (bp);
    bp += strlen (strcpy(bp, "\n"));
    i = bp - buffer;
    if (write (bb_fderr, buffer, i) != i)
	adios (EX_IOERR, bb_rept, "error writing");

    return NOTOK;
}

/*  */

static  notify (fd)
int     fd;
{
    int     i;
    char    buffer[BUFSIZ];

    if (bb_fderr == NOTOK)
	return;

    if (rp_isbad (sm_init (NULLCP, NULLCP, 0, 0, 0, 0, 0))
	    || rp_isbad (sm_winit (S_MAIL, bb_from)))
	goto sm_err;

    switch (sm_wadr (bb_from, NULLCP, NULLCP)) {
	case RP_OK:
	    for (i = 0; bb[i]; i++) {
		(void) sprintf (buffer, "local-%s-request", bb[i] -> bb_name);
		(void) sm_wadr (buffer, LocalName (), NULLCP);
	    }
	    break;

	default:
	    goto sm_err;
    }

    if (rp_isbad (sm_waend ()))
	goto sm_err;

    (void) sprintf (buffer,
	    "Date: %s\nFrom: %s\nTo: %s\nSubject: BBoards Failure\n\n",
	    dtimenow (), bb_from, bb_from);
    if (rp_isbad (sm_wtxt (buffer, strlen (buffer))))
	goto sm_err;

    for (i = 0; bb[i]; i++) {
	(void) sprintf (buffer, "BBoard %s\n", bb[i] -> bb_name);
	if (rp_isbad (sm_wtxt (buffer, strlen (buffer))))
	    goto sm_err;
    }

    (void) lseek (bb_fderr, (off_t)0, 0);
    while ((i = read (bb_fderr, buffer, sizeof buffer)) > 0)
	if (rp_isbad (sm_wtxt (buffer, i)))
	    goto sm_err;

    (void) strcpy (buffer, "\n------- Forwarded Message\n\n");
    if (rp_isbad (sm_wtxt (buffer, strlen (buffer))) || encap (fd) == NOTOK)
	goto sm_err;
    (void) strcpy (buffer, "\n------- End of Forwarded Message\n\n");
    if (rp_isbad (sm_wtxt (buffer, strlen (buffer))))
	goto sm_err;

    switch (sm_wtend ()) {
	case RP_OK:
	    (void) unlink (bb_rept);
	    (void) sm_end (OK);
	    return;

	default:
    sm_err: ;
	    adios (EX_UNAVAILABLE, NULLCP,
		    "failed and unable to post advisory, see %s for details",
		    bb_rept);
    }
}

/*  */

/* very similar to sbr/cpydgst.c */

#define	S1	0
#define	S2	1

#define	output(c)	if (bp >= dp) flush (), *bp++ = c; else *bp++ = c
#define	flush()		if ((j = bp - outbuf) \
				&& rp_isbad (sm_wtxt (outbuf, j))) \
			    return NOTOK; \
			else \
			    bp = outbuf

static int  encap (fd)
register int    fd;
{
    register int    i,
                    state;
    register char  *cp,
                   *ep;
    char    buffer[BUFSIZ];
    register int    j;
    register char  *bp,
                   *dp;
    char    outbuf[BUFSIZ];

    (void) lseek (fd, (off_t)0, 0);

    dp = (bp = outbuf) + sizeof outbuf;
    for (state = S1; (i = read (fd, buffer, sizeof buffer)) > 0;)
	for (ep = (cp = buffer) + i; cp < ep; cp++) {
	    if (*cp == NULL)
		continue;
	    switch (state) {
		case S1: 
		    if (*cp == '-') {
			output ('-');
			output (' ');
		    }
		    state = S2;	/* fall */

		case S2: 
		    output (*cp);
		    if (*cp == '\n')
			state = S1;
		    break;
	    }
	}

    if (i == NOTOK)
	return NOTOK;
    flush ();

    return OK;
}
#endif	DISTRIBUTE

/*  */

#ifndef	DISTRIBUTE
/* VARARGS1 */

static int  lose (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    adios (EX_UNAVAILABLE, NULLCP, fmt, a, b, c, d);/* NOTREACHED */
}
#endif	not DISTRIBUTE

/*  */

static  arginit (vec)
register char  **vec;
{
    register int    i;
#ifdef	MHMTS
    register char  *ap;
#endif	MHMTS
    char    addr[BUFSIZ];
    register struct bboard *bp;
    register struct passwd *pw;

    invo_name = r1bindex (*vec++, '/');
    m_foil (NULLCP);
    mts_init (invo_name);

#ifndef	SPOP
    if ((pw = getpwnam (BBOARDS)) == NULL)
	adios (EX_OSFILE, NULLCP, "no entry for ~%s", BBOARDS);
#else	SPOP
    if ((pw = getpwnam (POPUID)) == NULL || !setpwinfo (pw, POPDB, 1))
	adios (EX_OSFILE, NULLCP, "%s", pw ? getbberr () : "POP user-id unknown");
#endif	SPOP

    if (pw -> pw_uid != geteuid ())
#ifndef SPOP
    adios (EX_OSERR, NULLCP, "not running setuid to %s", BBOARDS);
#else	SPOP
    adios (EX_OSERR, NULLCP, "not running setuid to %s", POPUID);
#endif	SPOP

    bb_uid = pw -> pw_uid;
    bb_gid = pw -> pw_gid;
#ifndef	SPOP
    (void) strcpy (bb_from, adrsprintf (pw -> pw_name, LocalName ()));
    (void) strcpy (bb_home, pw -> pw_dir);
#endif	not SPOP

#ifdef	MHMTS
    vec += 3;
#endif	MHMTS
    if (*vec == NULL)
	adios (EX_USAGE, NULLCP, "usage: %s %s [%s ...]",
		invo_name, ENTITY, ENTITY);

    for (i = 0; *vec; vec++) {
#ifdef	MHMTS
	if (ap = index (*vec, '.'))
	    *vec = ++ap;
#endif	MHMTS
	make_lower (addr, *vec);

	if ((bp = getbbnam (addr)) == NULL
		&& (bp = getbbaka (addr)) == NULL)
	    adios (EX_NOUSER, NULLCP, "no such %s as %s", ENTITY, *vec);
	if ((bb[i++] = getbbcpy (bp)) == NULL)
	    adios (EX_UNAVAILABLE, NULLCP, "insufficient memory on %s", *vec);

	if (i >= NBB - 1)
	    adios (EX_USAGE, NULLCP, "too many %ss, starting with %s",
		    ENTITY, *vec);
    }
    bb[i] = NULL;

    (void) umask (0022);

    bb_fderr = NOTOK;
}

/*  */

static int  copyfile (qd, tmpfil)
int     qd;
register char   *tmpfil;
{
    int     i,
            fd;
    char    buffer[BUFSIZ];

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	adios (EX_CANTCREAT, tmpfil, "unable to create");
    (void) close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK)
	adios (EX_NOINPUT, tmpfil, "unable to re-open");

    (void) lseek (qd, (off_t)0, 0);
    while ((i = read (qd, buffer, sizeof buffer)) > 0)
	if (write (fd, buffer, i) != i)
	    adios (EX_IOERR, tmpfil, "error writing");
    if (i == NOTOK)
	adios (EX_IOERR, "input", "error reading");

    (void) lseek (fd, (off_t)0, 0);

    return fd;
}

/*  */

/* VARARGS3 */

#ifdef	MHMTS
/* ARGSUSED */
#endif	MHMTS

static  void adios (code, what, fmt, a, b, c, d, e, f)
int     code;
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    advise (what, fmt, a, b, c, d, e, f);
#ifdef	SENDMTS
    done (code);
#endif	SENDMTS
#ifdef	MHMTS
    done (1);
#endif	MHMTS
}
