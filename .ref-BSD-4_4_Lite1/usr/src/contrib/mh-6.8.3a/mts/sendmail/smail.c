/* smail.c - MH interface to SendMail/SMTP */
#ifndef	lint
static char ident[] = "@(#)$Id: smail.c,v 1.28 1994/03/23 23:24:51 jromine Exp $";
#endif

/* LINTLIBRARY */

/* This module implements an interface to SendMail very similar to the
   MMDF mm_(3) routines.  The sm_() routines herein talk SMTP to a
   sendmail process, mapping SMTP reply codes into RP_-style codes.
 */

#ifdef	BSD42
/* Under 4.2BSD, the alarm handing stuff for time-outs will NOT work due to
   the way syscalls get restarted.  This really is not crucial, since we
   expect SendMail to be well-behaved and not hang on us.  The only time
   I've ever seen Sendmail hang was with a bogus configuration file...
 */
#endif	/* BSD42 */
#ifdef	SENDMAILBUG
/*
 * It appears that some versions of Sendmail will return Code 451
 * when they don't really want to indicate a failure.
 * "Code 451 almost always means sendmail has deferred; we don't
 * really want bomb out at this point since sendmail will rectify
 * things later."  So, if you define SENDMAILBUG, Code 451 is
 * considered the same as Code 250.  Yuck!
 */
#ifndef	SENDMAIL
#undef		SENDMAILBUG
#endif	/* not SENDMAIL */
#endif	/* SENDMAILBUG */

#ifdef hpux	
/* HP-UX has this capability. It also handles (some) signals. */
#define BSD42 
#endif /* hpux */

#if	!defined(BSD42) && !defined(SOCKETS)
#undef	SMTP
#endif	/* not BSD42 and not SOCKETS */
#ifdef	SMTP
#undef	SENDMAIL
#endif	/* SMTP */


#include "../h/strings.h"
#include <stdio.h>
#include "smail.h"
#include "../zotnet/mts.h"
#include <ctype.h>
#include <signal.h>

#ifndef	TYPESIG
#define	TYPESIG	int
#endif

#define	NOTOK	(-1)
#define	OK	0
#define	DONE	1

#define	TRUE	1
#define	FALSE	0

#define	NBITS	((sizeof (int)) * 8)

#define	min(a,b)	((a) < (b) ? (a) : (b))


		/* these codes must all be different! */
#define	SM_OPEN	 90      /* Changed from 30 in case of nameserver flakiness */
#define	SM_HELO	 20
#define	SM_RSET	 15
#define	SM_MAIL	 40
#define	SM_RCPT	120
#define	SM_DATA	 20
#define	SM_TEXT	150
#define	SM_DOT	180
#define	SM_QUIT	 30
#define	SM_CLOS	 10

/*  */

static TYPESIG	alrmser ();

static int  sm_addrs = 0;
static int  sm_alarmed = 0;
#ifndef	SMTP
static int  sm_child = NOTOK;
#endif	/* not SMTP */
static int  sm_debug = 0;
static int  sm_nl = TRUE;
static int  sm_verbose = 0;

static  FILE * sm_rfp = NULL;
static  FILE * sm_wfp = NULL;

#ifdef	MPOP
static	int  sm_ispool = 0;
static	char sm_tmpfil[BUFSIZ];
#endif /* MPOP */

static char *sm_noreply = "No reply text given";
static char *sm_moreply = "; ";

struct smtp sm_reply;		/* global... */


void	discard ();
char   *r1bindex ();

static int	rclient(), sm_ierror(), smtalk(), sm_wrecord(), sm_wstream();
static int	sm_werror(), smhear(), sm_rrecord(), sm_rerror();

#ifdef	MPOP
extern	int	errno;
#ifndef	BSD44
extern	int	sys_nerr;
extern	char   *sys_errlist[];
#endif
extern	char  **brkstring (), **copyip (), *getcpy ();
#endif


static	int	doingEHLO;

#define	MAXEHLO	10
char   *EHLOkeys[MAXEHLO + 1];

char   *EHLOset ();

/*  */

#ifndef	SMTP

/* ARGSUSED */

int     sm_init (client, server, watch, verbose, debug, onex, queued)
char   *client,
       *server;
int	watch,
	verbose,
	debug,
	onex,
    	queued;
{
    register int    i,
                    result,
                    vecp;
    int     pdi[2],
            pdo[2];
    char   *vec[15];

    if (watch)
#ifndef	SUN40
	verbose = TRUE;
#else	/* to show the transaction, -watch must imply -snoop */
	debug = verbose = TRUE;
#endif
    sm_verbose = verbose;
    sm_debug = debug;
    if (sm_rfp != NULL && sm_wfp != NULL)
	return RP_OK;

    if (client == NULL || *client == '\0')
	client = clientname;
#ifdef ZMAILER
    if (client == NULL || *client == '\0')
	client = "localhost";
#endif

    if (pipe (pdi) == NOTOK)
	return sm_ierror ("no pipes");
    if (pipe (pdo) == NOTOK) {
	(void) close (pdi[0]);
	(void) close (pdi[1]);
	return sm_ierror ("no pipes");
    }

    for (i = 0; (sm_child = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (sm_child) {
	case NOTOK: 
	    (void) close (pdo[0]);
	    (void) close (pdo[1]);
	    (void) close (pdi[0]);
	    (void) close (pdi[1]);
	    return sm_ierror ("unable to fork");

	case OK: 
	    if (pdo[0] != fileno (stdin))
		(void) dup2 (pdo[0], fileno (stdin));
	    if (pdi[1] != fileno (stdout))
		(void) dup2 (pdi[1], fileno (stdout));
	    if (pdi[1] != fileno (stderr))
		(void) dup2 (pdi[1], fileno (stderr));
	    for (i = fileno (stderr) + 1; i < NBITS; i++)
		(void) close (i);

	    vecp = 0;
	    vec[vecp++] = r1bindex (sendmail, '/');
	    vec[vecp++] = "-bs";
#ifndef ZMAILER
	    vec[vecp++] = watch ? "-odi" : queued ? "-odq" : "-odb";
	    vec[vecp++] = "-oem";
	    vec[vecp++] = "-om";
#ifndef	RAND
	    if (verbose)
		vec[vecp++] = "-ov";
#endif	/* not RAND */
#endif	/* not ZMAILER */
	    vec[vecp++] = NULL;

	    (void) setgid (getegid ());
	    (void) setuid (geteuid ());
	    execvp (sendmail, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (sendmail);
	    _exit (-1);		/* NOTREACHED */

	default: 
	    (void) signal (SIGALRM, alrmser);
	    (void) signal (SIGPIPE, SIG_IGN);

	    (void) close (pdi[1]);
	    (void) close (pdo[0]);
	    if ((sm_rfp = fdopen (pdi[0], "r")) == NULL
		    || (sm_wfp = fdopen (pdo[1], "w")) == NULL) {
		(void) close (pdi[0]);
		(void) close (pdo[1]);
		sm_rfp = sm_wfp = NULL;
		return sm_ierror ("unable to fdopen");
	    }
	    sm_alarmed = 0;
	    (void) alarm (SM_OPEN);
	    result = smhear ();
	    (void) alarm (0);
	    switch (result) {
		case 220: 
		    break;

		default: 
		    (void) sm_end (NOTOK);
		    return RP_RPLY;
	    }

	    if (client && *client) {
		doingEHLO = 1;
		result = smtalk (SM_HELO, "EHLO %s", client);
		doingEHLO = 0;

		if (500 <= result && result <= 599)
		    result = smtalk (SM_HELO, "HELO %s", client);

		switch (result) {
		    case 250:
		        break;

		    default:
			(void) sm_end (NOTOK);
			return RP_RPLY;
		}
	    }

#ifndef	ZMAILER
	    if (onex)
		(void) smtalk (SM_HELO, "ONEX");
#endif
	    if (watch)
		(void) smtalk (SM_HELO, "VERB on");

	    return RP_OK;
    }
}
#else	/* SMTP */

/*  */

int     sm_init (client, server, watch, verbose, debug, onex, queued)
char   *client,
       *server;
int	watch,
	verbose,
	debug,
	onex,
	queued;
{
    register int    result,
                    sd1,
                    sd2;

    if (watch)
#if	!defined(SUN40) || defined(MMDFII)
	verbose = TRUE;
#else	/* to show the transaction, -watch must imply -snoop */
	debug = verbose = TRUE;
#endif
    sm_verbose = verbose;
    sm_debug = debug;
#ifdef	MPOP
    if (sm_ispool)
	goto all_done;
#endif
    if (sm_rfp != NULL && sm_wfp != NULL)
	goto send_options;

    if (client == NULL || *client == '\0')
	if (clientname)
	    client = clientname;
	else
	    client = LocalName ();	/* no clientname -> LocalName */
#ifdef ZMAILER
    if (client == NULL || *client == '\0')
	client = "localhost";
#endif	/* not ZMAILER */

    if ((sd1 = rclient (server, "tcp", "smtp")) == NOTOK)
	return RP_BHST;
#ifdef	MPOP
    if (sm_ispool) {
	if (sm_rfp) {
	    (void) alarm (SM_CLOS);
	    (void) fclose (sm_rfp);
	    (void) alarm (0);
	    sm_rfp = NULL;
	}
	if ((sm_wfp = fdopen (sd1, "w")) == NULL) {
	    (void) unlink (sm_tmpfil);
	    (void) close (sd1);
	    return sm_ierror ("unable to fdopen");
	}
all_done: ;
	sm_reply.text[sm_reply.length = 0] = NULL;
	return (sm_reply.code = RP_OK);
    }
#endif /* MPOP */
    if ((sd2 = dup (sd1)) == NOTOK) {
	(void) close (sd1);
	return sm_ierror ("unable to dup");
    }

    (void) signal (SIGALRM, alrmser);
    (void) signal (SIGPIPE, SIG_IGN);

    if ((sm_rfp = fdopen (sd1, "r")) == NULL
	    || (sm_wfp = fdopen (sd2, "w")) == NULL) {
	(void) close (sd1);
	(void) close (sd2);
	sm_rfp = sm_wfp = NULL;
	return sm_ierror ("unable to fdopen");
    }
    sm_alarmed = 0;
    (void) alarm (SM_OPEN);
    result = smhear ();
    (void) alarm (0);
    switch (result) {
	case 220: 
	    break;

	default: 
	    (void) sm_end (NOTOK);
	    return RP_RPLY;
    }

    if (client && *client) {
	doingEHLO = 1;
	result = smtalk (SM_HELO, "EHLO %s", client);
	doingEHLO = 0;

	if (500 <= result && result <= 599)
	    result = smtalk (SM_HELO, "HELO %s", client);

	switch (result) {
	    case 250: 
		break;

	    default: 
		(void) sm_end (NOTOK);
		return RP_RPLY;
	}
    }

send_options: ;
    if (watch && EHLOset ("XVRB"))
	(void) smtalk (SM_HELO, "VERB on");
    if (onex && EHLOset ("XONE"))
	(void) smtalk (SM_HELO, "ONEX");
    if (queued && EHLOset ("XQUE"))
	(void) smtalk (SM_HELO, "QUED");

    return RP_OK;
}


#ifdef	MPOP
#define	MAXARGS	1000
#endif /* MPOP */

static int rclient (server, protocol, service)
char   *server,
       *protocol,
       *service;
{
    int     sd;
    char    response[BUFSIZ];
#ifdef	MPOP
    char   *cp;
#endif /* MPOP */

    if ((sd = client (server, protocol, service, FALSE, response)) != NOTOK)
	return sd;

#ifdef	MPOP
    if (!server && servers && (cp = index (servers, '/'))) {
	register char  **ap;
	char   *arguments[MAXARGS];

	(void) copyip (brkstring (cp = getcpy (servers), " ", "\n"),
		       arguments);

	for (ap = arguments; *ap; ap++)
	    if (**ap == '/') {
		register char *dp;

		if ((dp = rindex (*ap, '/')) && *++dp == NULL)
		    *--dp = NULL;
		(void) sprintf (sm_tmpfil, "%s/smtpXXXXXX", *ap);
		(void) mktemp (sm_tmpfil);

		if ((sd = creat (sm_tmpfil, 0600)) != NOTOK) {
		    sm_ispool = 1;
		    break;
		}
	    }

	free (cp);
	if (sd != NOTOK)
	    return sd;
    }
#endif /* MPOP */

    (void) sm_ierror ("%s", response);
    return NOTOK;
}
#endif	/* SMTP */

/*  */

int     sm_winit (mode, from)
register int	mode;
register char   *from;
{
#ifdef	MPOP
    if (sm_ispool && !sm_wfp) {
	(void) strlen (strcpy (sm_reply.text,
			       "unable to create new spool file"));
	sm_reply.code = NOTOK;
	return RP_BHST;
    }
#endif /* MPOP */

    switch (smtalk (SM_MAIL, "%s FROM:<%s>",
		mode == S_SEND ? "SEND" : mode == S_SOML ? "SOML"
		: mode == S_SAML ? "SAML" : "MAIL", from)) {
	case 250: 
	    sm_addrs = 0;
	    return RP_OK;

	case 500: 
	case 501: 
	case 552: 
	    return RP_PARM;

	default: 
	    return RP_RPLY;
    }
}

/*  */

#ifdef	BERK
/* ARGUSED */
#endif	/* BERK */

int     sm_wadr (mbox, host, path)
register char   *mbox;
#ifndef	BERK
register
#endif	/* not BERK */
	 char   *host,
		*path;
{
#ifndef	BERK
    switch (smtalk (SM_RCPT, host && *host ? "RCPT TO:<%s%s@%s>"
					   : "RCPT TO:<%s%s>",
			     path ? path : "", mbox, host)) {
#else	/* BERK */
    switch (smtalk (SM_RCPT, "RCPT TO:%s", mbox)) {
#endif	/* BERK */
	case 250: 
	case 251: 
	    sm_addrs++;
	    return RP_OK;

	case 451: 
#ifdef SENDMAILBUG
	    sm_addrs++;
	    return RP_OK;
#endif /* SENDMAILBUG */
	case 421: 
	case 450: 
	case 452: 
	    return RP_NO;

	case 500: 
	case 501: 
	    return RP_PARM;

	case 550: 
	case 551: 
	case 552: 
	case 553: 
	    return RP_USER;

	default: 
	    return RP_RPLY;
    }
}

/*  */

int     sm_waend () {
    switch (smtalk (SM_DATA, "DATA")) {
	case 354: 
	    sm_nl = TRUE;
	    return RP_OK;

	case 451: 
#ifdef SENDMAILBUG
	    sm_nl = TRUE;
	    return RP_OK;
#endif /* SENDMAILBUG */
	case 421: 
	    return RP_NO;

	case 500: 
	case 501: 
	case 503: 
	case 554: 
	    return RP_NDEL;

	default: 
	    return RP_RPLY;
    }
}

/*  */

int     sm_wtxt (buffer, len)
register char   *buffer;
register int     len;
{
    register int    result;

    sm_alarmed = 0;
    (void) alarm (SM_TEXT);
    result = sm_wstream (buffer, len);
    (void) alarm (0);

    return (result == NOTOK ? RP_BHST : RP_OK);
}

/*  */

int     sm_wtend () {
    if (sm_wstream ((char *) NULL, 0) == NOTOK)
	return RP_BHST;

    switch (smtalk (SM_DOT + 3 * sm_addrs, ".")) {
	case 250: 
	case 251: 
	    return RP_OK;

	case 451: 
#ifdef SENDMAILBUG
	    return RP_OK;
#endif /* SENDMAILBUG */
	case 452: 
	default: 
	    return RP_NO;

	case 552: 
	case 554: 
	    return RP_NDEL;
    }
}

/*  */

int     sm_end (type)
register int     type;
{
    register int    status;
    struct smtp sm_note;

#ifndef	SMTP
    switch (sm_child) {
	case NOTOK: 
	case OK: 
	    return RP_OK;

	default: 
	    break;
    }
#endif	/* not SMTP */
    if (sm_rfp == NULL && sm_wfp == NULL)
	return RP_OK;

    switch (type) {
	case OK: 
	    (void) smtalk (SM_QUIT, "QUIT");
	    break;

	case NOTOK: 
	    sm_note.code = sm_reply.code;
	    (void) strncpy (sm_note.text, sm_reply.text,
		    sm_note.length = sm_reply.length);/* fall */
	case DONE: 
	    if (smtalk (SM_RSET, "RSET") == 250 && type == DONE)
		return RP_OK;
#ifndef	SMTP
	    (void) kill (sm_child, SIGKILL);
	    discard (sm_rfp);
	    discard (sm_wfp);
#else	/* SMTP */
	    (void) smtalk (SM_QUIT, "QUIT");
#endif	/* not SMTP */
	    if (type == NOTOK) {
		sm_reply.code = sm_note.code;
		(void) strncpy (sm_reply.text, sm_note.text,
			sm_reply.length = sm_note.length);
	    }
	    break;
    }
#ifdef	MPOP
#ifdef	SMTP
    if (sm_ispool) {
	sm_ispool = 0;

	if (sm_wfp) {
	    (void) unlink (sm_tmpfil);
	    (void) fclose (sm_wfp);
	    sm_wfp = NULL;
	}
    }
#endif
#endif /* MPOP */
    if (sm_rfp != NULL) {
	(void) alarm (SM_CLOS);
	(void) fclose (sm_rfp);
	(void) alarm (0);
    }
    if (sm_wfp != NULL) {
	(void) alarm (SM_CLOS);
	(void) fclose (sm_wfp);
	(void) alarm (0);
    }

#ifndef	SMTP
    status = pidwait (sm_child);

    sm_child = NOTOK;
#else	/* SMTP */
    status = 0;
#endif	/* SMTP */
    sm_rfp = sm_wfp = NULL;

    return (status ? RP_BHST : RP_OK);
}

/*  */

#ifdef	MPOP
#ifdef	SMTP
#include <sys/types.h>
#include <sys/stat.h>


int	sm_bulk (file)
char   *file;
{
    int	    cc,
	    i,
	    j,
	    k,
	    result;
    long    pos;
    register char *dp;
    char   *bp,
	   *cp,
	    buffer[BUFSIZ],
	    sender[BUFSIZ];
    FILE   *fp,
	   *gp;

    gp = NULL;
    k = strlen (file) - sizeof ".bulk";
    if ((fp = fopen (file, "r")) == NULL) {
	(void) sprintf (bp = sm_reply.text, "unable to read %s: ", file);
	bp += strlen (bp);
	if (errno > 0 && errno < sys_nerr)
	    (void) sprintf (bp, "%s", sys_errlist[errno]);
	else
	    (void) sprintf (bp, "Error %d", errno);
	sm_reply.length = strlen (sm_reply.text);
	sm_reply.code = NOTOK;
	return RP_BHST;
    }
    if (sm_debug) {
	printf ("reading file %s\n", file);
	(void) fflush (stdout);
    }

    i = j = 0;
    while (fgets (buffer, sizeof buffer, fp)) {
	if (j++ == 0)
	    (void) strcpy (sender, buffer + sizeof "MAIL FROM:" - 1);
	if (strcmp (buffer, "DATA\r\n") == 0) {
	    i = 1;
	    break;
	}
    }
    if (i == 0) {
	if (sm_debug) {
	    printf ("no DATA...\n");
	    (void) fflush (stdout);
	}
losing0: ;
	(void) sprintf (buffer, "%s.bad", file);
	(void) rename (file, buffer);
	if (gp) {
	    (void) sprintf (buffer, "%*.*sA.bulk", k, k, file);
	    (void) unlink (buffer);
	    (void) fclose (gp);
	}
	(void) fclose (fp);
	return RP_OK;
    }
    if (j < 3) {
	if (sm_debug) {
	    printf ("no %srecipients...\n", j < 1 ? "sender or " : "");
	    (void) fflush (stdout);
	}
	goto losing0;
    }

    if ((cp = malloc ((unsigned) (cc = (pos = ftell (fp)) + 1))) == NULL) {
	sm_reply.length = strlen (strcpy (sm_reply.text, "out of memory"));
losing1: ;
	sm_reply.code = NOTOK;
	(void) fclose (fp);
	return RP_BHST;
    }
    (void) fseek (fp, 0L, 0);
    for (dp = cp, i = 0; i++ < j; dp += strlen (dp))
	if (fgets (dp, cc - (dp - cp), fp) == NULL) {
	    sm_reply.length = strlen (strcpy (sm_reply.text, "premature eof"));
losing2: ;
	    free (cp);
	    goto losing1;
	}
    *dp = NULL;

    for (dp = cp, i = cc - 1; i > 0; dp += cc, i -= cc)
	if ((cc = write (fileno (sm_wfp), dp, i)) == NOTOK) {
losing3: ;
	    (void) strcpy (bp = sm_reply.text, "error writing to server: ");
	    bp += strlen (bp);
	    if (errno > 0 && errno < sys_nerr)
		(void) sprintf (bp, "%s", sys_errlist[errno]);
	    else
		(void) sprintf (bp, "Error %d", errno);
	    sm_reply.length = strlen (sm_reply.text);
	    goto losing2;
	}
	else
	    if (sm_debug) {
		printf ("wrote %d octets to server\n", cc);
		(void) fflush (stdout);
	    }

    for (dp = cp, i = 0; i++ < j; dp = index (dp, '\n'), dp++) {
	if (sm_debug) {
	    if (bp = index (dp, '\r'))
		*bp = NULL;
	    printf ("=> %s\n", dp);
	    (void) fflush (stdout);
	    if (bp)
		*bp = '\r';
	}

	switch (smhear () + (i == 1 ? 1000 : i != j ? 2000 : 3000)) {
	    case 1000 + 250:
	        sm_addrs = 0;
	        result = RP_OK;
		break;

	    case 1000 + 500: 
	    case 1000 + 501: 
	    case 1000 + 552: 
	    case 2000 + 500: 
	    case 2000 + 501:
		result = RP_PARM;
		(void) smtalk (SM_RSET, "RSET");
		free (cp);
		goto losing0;

	    case 2000 + 250:
	    case 2000 + 251:
		sm_addrs++;
	        result = RP_OK;
		break;

	    case 2000 + 451: 
#ifdef SENDMAILBUG
		sm_addrs++;
		result = RP_OK;
		break;
#endif
	    case 2000 + 421: 
	    case 2000 + 450: 
	    case 2000 + 452: 
		result = RP_NO;
		goto bad_addr;

	    case 2000 + 550: 
	    case 2000 + 551: 
	    case 2000 + 552: 
	    case 2000 + 553: 
		result = RP_USER;
bad_addr: ;
		if (k <= 0 || strcmp (sender, "<>\r\n") == 0)
		    break;
		if (gp == NULL) {
		    int	    l;
		    (void) sprintf (buffer, "%*.*sA.bulk", k, k, file);
		    if ((gp = fopen (buffer, "w+")) == NULL)
			goto bad_data;
		    fprintf (gp, "MAIL FROM:<>\r\nRCPT TO:%sDATA\r\n", sender);
		    l = strlen (sender);
		    fprintf (gp,
			     "To: %*.*s\r\nSubject: Invalid addresses (%s)\r\n",
			     l - 4, l - 4, sender + 1, file);
		    fprintf (gp, "Date: %s\r\nFrom: Postmaster@%s\r\n\r\n",
			     dtimenow (), LocalName ());
		}
		if (bp = index (dp, '\r'))
		    *bp = NULL;
		fprintf (gp, "=>        %s\r\n", dp);
		if (bp)
		    *bp = '\r';
		fprintf (gp, "<= %s\r\n", rp_string (result));
		(void) fflush (gp);
		break;

	    case 3000 + 354: 
#ifdef SENDMAILBUG
ok_data: ;
#endif
		result = RP_OK;
		break;

	    case 3000 + 451: 
#ifdef SENDMAILBUG
		goto ok_data;
#endif
	    case 3000 + 421:
		result = RP_NO;
bad_data: ;
		(void) smtalk (SM_RSET, "RSET");
		free (cp);
		if (gp) {
		    (void) sprintf (buffer, "%*.*sA.bulk", k, k, file);
		    (void) unlink (buffer);
		    (void) fclose (gp);
		}
		(void) fclose (fp);
		return result;

	    case 3000 + 500: 
	    case 3000 + 501: 
	    case 3000 + 503: 
	    case 3000 + 554: 
		(void) smtalk (SM_RSET, "RSET");
		free (cp);
		goto no_dice;

	    default:
		result = RP_RPLY;
		goto bad_data;
	}
    }
    free (cp);

    {
	struct stat st;

#ifdef SYS5
	cc = BUFSIZ;
#else
	if (fstat (fileno (sm_wfp), &st) == NOTOK
	        || (cc = st.st_blksize) < BUFSIZ)
	    cc = BUFSIZ;
#endif
	if ((cp = malloc ((unsigned) cc)) == NULL) {
	    (void) smtalk (SM_RSET, "RSET");
	    sm_reply.length = strlen (strcpy (sm_reply.text, "out of memory"));
	    goto losing1;
	}
    }
    (void) fseek (fp, pos, 0);
    for (;;) {
	int	eof = 0;

	for (dp = cp, i = cc; i > 0; dp += j, i -= j)
	    if ((j = fread (cp, sizeof *cp, i, fp)) == OK) {
		if (ferror (fp)) {
		    (void) sprintf (bp = sm_reply.text,
				    "error reading %s: ", file);
		    bp += strlen (bp);
		    if (errno > 0 && errno < sys_nerr)
			(void) sprintf (bp, "%s", sys_errlist[errno]);
		    else
			(void) sprintf (bp, "Error %d", errno);
		    sm_reply.length = strlen (sm_reply.text);
		    goto losing2;
		}
		cc = dp - cp;
		eof = 1;
		break;
	    }

	for (dp = cp, i = cc; i > 0; dp += j, i -= j)
	    if ((j = write (fileno (sm_wfp), dp, i)) == NOTOK)
		goto losing3;
	    else
		if (sm_debug) {
		    printf ("wrote %d octets to server\n", j);
		    (void) fflush (stdout);
		}

	if (eof)
	    break;
    }
    free (cp);

    switch (smhear ()) {
	case 250: 
	case 251: 
#ifdef SENDMAILBUG
ok_dot: ;
#endif
	    result = RP_OK;
	    (void) unlink (file);
	    break;

	case 451: 
#ifdef SENDMAILBUG
	    goto ok_dot;
#endif
	case 452: 
	default: 
	    result = RP_NO;
	    if (gp) {
		(void) sprintf (buffer, "%*.*sA.bulk", k, k, file);
		(void) unlink (buffer);
		(void) fclose (gp);
		gp = NULL;
	    }
	    break;

	case 552: 
	case 554: 
no_dice: ;
	    result = RP_NDEL;
	    if (k <= 0 || strcmp (sender, "<>\r\n") == 0) {
		(void) unlink (file);
		break;
	    }
	    if (gp) {
		(void) fflush (gp);
		(void) ftruncate (fileno (gp), 0L);
		(void) fseek (gp, 0L, 0);
	    }
    	    else {
		(void) sprintf (buffer, "%*.*sA.bulk", k, k, file);
		if ((gp = fopen (buffer, "w")) == NULL)
		    break;
	    }
	    fprintf (gp, "MAIL FROM:<>\r\nRCPT TO:%sDATA\r\n", sender);
	    i = strlen (sender);
	    fprintf (gp, "To: %*.*s\r\nSubject: Failed mail (%s)\r\n",
		     i - 4, i - 4, sender + 1, file);
            fprintf (gp, "Date: %s\r\nFrom: Postmaster@%s\r\n\r\n",
		     dtimenow (), LocalName ());
	    break;
    }

    if (gp) {
	(void) fputs ("\r\n------- Begin Returned message\r\n\r\n", gp);
	(void) fseek (fp, pos, 0);
	while (fgets (buffer, sizeof buffer, fp)) {
	    if (buffer[0] == '-')
		(void) fputs ("- ", gp);
	    if (strcmp (buffer, ".\r\n"))
		(void) fputs (buffer, gp);
	}
	(void) fputs ("\r\n------- End Returned Message\r\n\r\n.\r\n", gp);
	(void) fflush (gp);
	if (!ferror (gp))
	    (void) unlink (file);
	(void) fclose (gp);
    }
    (void) fclose (fp);

    return result;
}
#endif
#endif /* MPOP */

/*  */

/* VARARGS */

static int  sm_ierror (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    (void) sprintf (sm_reply.text, fmt, a, b, c, d);
    sm_reply.length = strlen (sm_reply.text);
    sm_reply.code = NOTOK;

    return RP_BHST;
}

/*  */

/* VARARGS2 */

static int  smtalk (time, fmt, a, b, c, d)
register int     time;
register char   *fmt;
{
    register int    result;
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, fmt, a, b, c, d);
    if (sm_debug) {
	printf ("=> %s\n", buffer);
	(void) fflush (stdout);
    }

#ifdef	MPOP
    if (sm_ispool) {
	char    file[BUFSIZ];

	if (strcmp (buffer, ".") == 0)
	    time = SM_DOT;
	fprintf (sm_wfp, "%s\r\n", buffer);
	switch (time) {
	    case SM_DOT:
	        (void) fflush (sm_wfp);
	        if (ferror (sm_wfp))
		    return sm_werror ();
		(void) sprintf (file, "%s%c.bulk", sm_tmpfil,
				(char) (sm_ispool + 'a' - 1));
		if (rename (sm_tmpfil, file) == NOTOK) {
		    char   *bp;
		    (void) sprintf (bp = sm_reply.text,
				    "error renaming %s to %s: ",
				    sm_tmpfil, file);
		    bp += strlen (bp);
		    if (errno > 0 && errno < sys_nerr)
			(void) sprintf (bp, "%s", sys_errlist[errno]);
		    else
			(void) sprintf (bp, "Error %d", errno);
		    sm_reply.length = strlen (sm_reply.text);
		    sm_reply.code = NOTOK;
		    return RP_BHST;
		}
		(void) fclose (sm_wfp);
		if (sm_wfp = fopen (sm_tmpfil, "w"))
		    (void) chmod (sm_tmpfil, 0600);
		sm_ispool++;
		/* and fall... */

	    case SM_MAIL:
	    case SM_RCPT:
	        result = 250;
		break;

	    case SM_RSET:
		(void) fflush (sm_wfp);
		(void) ftruncate (fileno (sm_wfp), 0L);
		(void) fseek (sm_wfp, 0L, 0);
	        result = 250;
		break;

	    case SM_DATA:
	        result = 354;
		break;

	    case SM_QUIT:
		(void) unlink (sm_tmpfil);
		sm_ispool = 0;
	        result = 221;
		break;

	    default:
		result = 500;
		break;
	}
	if (sm_debug) {
	    printf ("<= %d\n", result);
	    (void) fflush (stdout);
	}

	sm_reply.text[sm_reply.length = 0] = NULL;
	return (sm_reply.code = result);
    }
#endif /* MPOP */

    sm_alarmed = 0;
    (void) alarm ((unsigned) time);
    if ((result = sm_wrecord (buffer, strlen (buffer))) != NOTOK)
	result = smhear ();
    (void) alarm (0);

    return result;
}

/*  */

static int  sm_wrecord (buffer, len)
register char   *buffer;
register int     len;
{
    if (sm_wfp == NULL)
	return sm_werror ();

    (void) fwrite (buffer, sizeof *buffer, len, sm_wfp);
    (void) fputs ("\r\n", sm_wfp);
    (void) fflush (sm_wfp);

    return (ferror (sm_wfp) ? sm_werror () : OK);
}

/*  */

static int  sm_wstream (buffer, len)
register char   *buffer;
register int     len;
{
    register char  *bp;
    static char lc = 0;

    if (sm_wfp == NULL)
	return sm_werror ();

    if (buffer == NULL && len == 0) {
	if (lc != '\n')
	    (void) fputs ("\r\n", sm_wfp);
	lc = 0;
	return (ferror (sm_wfp) ? sm_werror () : OK);
    }

    for (bp = buffer; len > 0; bp++, len--) {
	switch (*bp) {
	    case '\n': 
		sm_nl = TRUE;
		(void) fputc ('\r', sm_wfp);
		break;

	    case '.': 
		if (sm_nl)
		    (void) fputc ('.', sm_wfp);/* FALL THROUGH */
	    default: 
		sm_nl = FALSE;
	}
	(void) fputc (*bp, sm_wfp);
	if (ferror (sm_wfp))
	    return sm_werror ();
    }

    if (bp > buffer)
	lc = *--bp;
    return (ferror (sm_wfp) ? sm_werror () : OK);
}

/*  */

#ifdef _AIX
/*
 * AIX by default will inline the strlen and strcpy commands by redefining
 * them as __strlen and __strcpy respectively.  This causes compile problems
 * with the #ifdef MPOP in the middle.  Should the #ifdef MPOP be removed,
 * remove these #undefs.
 */
#undef strlen
#undef strcpy
#endif /* _AIX */

static int  sm_werror () {
    sm_reply.length =
#ifdef	SMTP
	strlen (strcpy (sm_reply.text, sm_wfp == NULL ? "no socket opened"
	    : sm_alarmed ? "write to socket timed out"
#ifdef	MPOP
	    : sm_ispool ? "error writing to spool file"
#endif
	    : "error writing to socket"));
#else	/* !SMTP */
	strlen (strcpy (sm_reply.text, sm_wfp == NULL ? "no pipe opened"
	    : sm_alarmed ? "write to pipe timed out"
	    : "error writing to pipe"));
#endif	/* !SMTP */

    return (sm_reply.code = NOTOK);
}

/*  */

static int  smhear () {
    register int    i,
                    code,
                    cont,
		    rc,
		    more;
    int     bc;
    register char  *bp,
                   *rp;
    char  **ehlo,
	    buffer[BUFSIZ];

    if (doingEHLO) {
	static	int	at_least_once = 0;

	if (at_least_once) {
	    char   *ep;

	    for (ehlo = EHLOkeys; ep = *ehlo; ehlo++)
		free (ep);
	}
	else
	    at_least_once = 1;

	*(ehlo = EHLOkeys) = NULL;
    }

again: ;

    sm_reply.text[sm_reply.length = 0] = 0;

    rp = sm_reply.text, rc = sizeof sm_reply.text - 1;
    for (more = FALSE; sm_rrecord (bp = buffer, &bc) != NOTOK;) {
	if (sm_debug) {
	    printf ("<= %s\n", buffer);
	    (void) fflush (stdout);
	}

	if (doingEHLO
	        && strncmp (buffer, "250", sizeof "250" - 1) == 0
	        && (buffer[3] == '-' || doingEHLO == 2)
	        && buffer[4]) {
	    if (doingEHLO == 2) {
		int	len = strlen (buffer + 4);

		if (*ehlo = malloc ((unsigned) (strlen (buffer + 4) + 1))) {
		    (void) strcpy (*ehlo++, buffer + 4);
		    *ehlo = NULL;
		    if (ehlo >= EHLOkeys + MAXEHLO)
			doingEHLO = 0;
		}
		else
		    doingEHLO = 0;
	    }
	    else
		doingEHLO = 2;
	}

	for (; bc > 0 && (!isascii (*bp) || !isdigit (*bp)); bp++, bc--)
	    continue;

	cont = FALSE;
	code = atoi (bp);
	bp += 3, bc -= 3;
	for (; bc > 0 && isspace (*bp); bp++, bc--)
	    continue;
	if (bc > 0 && *bp == '-') {
	    cont = TRUE;
	    bp++, bc--;
	    for (; bc > 0 && isspace (*bp); bp++, bc--)
		continue;
	}

	if (more) {
	    if (code != sm_reply.code || cont)
		continue;
	    more = FALSE;
	}
	else {
	    sm_reply.code = code;
	    more = cont;
	    if (bc <= 0) {
		(void) strcpy (bp = buffer, sm_noreply);
		bc = strlen (sm_noreply);
	    }
	}
	if ((i = min (bc, rc)) > 0) {
	    (void) strncpy (rp, bp, i);
	    rp += i, rc -= i;
	    if (more && rc > strlen (sm_moreply) + 1) {
		(void) strcpy (sm_reply.text + rc, sm_moreply);
		rc += strlen (sm_moreply);
	    }
	}
	if (more)
	    continue;
	if (sm_reply.code < 100) {
	    if (sm_verbose) {
		printf ("%s\n", sm_reply.text);
		(void) fflush (stdout);
	    }
	    goto again;
	}

	sm_reply.length = rp - sm_reply.text;

	return sm_reply.code;
    }

    return NOTOK;
}

/*  */

static int  sm_rrecord (buffer, len)
register char   *buffer;
register int    *len;
{
    if (sm_rfp == NULL)
	return sm_rerror ();

    buffer[*len = 0] = 0;

    (void) fgets (buffer, BUFSIZ, sm_rfp);
    *len = strlen (buffer);
    if (ferror (sm_rfp) || feof (sm_rfp))
	return sm_rerror ();
    if (buffer[*len - 1] != '\n')
	while (getc (sm_rfp) != '\n' && !ferror (sm_rfp) && !feof (sm_rfp))
	    continue;
    else
	if (buffer[*len - 2] == '\r')
	    *len -= 1;
    buffer[*len - 1] = 0;

    return OK;
}

/*  */

static int  sm_rerror () {
    sm_reply.length =
#ifdef	SMTP
	strlen (strcpy (sm_reply.text, sm_rfp == NULL ? "no socket opened"
	    : sm_alarmed ? "read from socket timed out"
	    : feof (sm_rfp) ? "premature end-of-file on socket"
	    : "error reading from socket"));
#else	/* not SMTP */
	strlen (strcpy (sm_reply.text, sm_rfp == NULL ? "no pipe opened"
	    : sm_alarmed ? "read from pipe timed out"
	    : feof (sm_rfp) ? "premature end-of-file on pipe"
	    : "error reading from pipe"));
#endif	/* not SMTP */

    return (sm_reply.code = NOTOK);
}

/*  */

/* ARGSUSED */

static	TYPESIG alrmser (i)
int     i;
{
#ifndef	BSD42
    signal (SIGALRM, alrmser);
#endif	/* BSD42 */
    sm_alarmed++;

    if (sm_debug) {
	printf ("timed out...\n");
	(void) fflush (stdout);
    }
}

/*  */

char   *rp_string (code)
register int     code;
{
    register char  *text;
    static char buffer[BUFSIZ];

    switch (sm_reply.code != NOTOK ? code : NOTOK) {
	case RP_AOK:
	    text = "AOK";
	    break;

	case RP_MOK:
	    text = "MOK";
	    break;

	case RP_OK: 
	    text = "OK";
	    break;

	case RP_RPLY: 
	    text = "RPLY";
	    break;

	case RP_BHST: 
	default: 
	    text = "BHST";
	    (void) sprintf (buffer, "[%s] %s", text, sm_reply.text);
	    return buffer;

	case RP_PARM: 
	    text = "PARM";
	    break;

	case RP_NO: 
	    text = "NO";
	    break;

	case RP_USER: 
	    text = "USER";
	    break;

	case RP_NDEL: 
	    text = "NDEL";
	    break;
    }

    (void) sprintf (buffer, "[%s] %3d %s", text, sm_reply.code, sm_reply.text);
    return buffer;
}

/*  */

#ifdef	MPOP
#ifdef	SMTP
static char *broken[MAXARGS + 1];


static char **brkstring (strg, brksep, brkterm)
register char  *strg;
register char  *brksep,
               *brkterm;
{
    register int    bi;
    register char   c,
                   *sp;

    sp = strg;

    for (bi = 0; bi < MAXARGS; bi++) {
	while (brkany (c = *sp, brksep))
	    *sp++ = 0;
	if (!c || brkany (c, brkterm)) {
	    *sp = 0;
	    broken[bi] = 0;
	    return broken;
	}

	broken[bi] = sp;
	while ((c = *++sp) && !brkany (c, brksep) && !brkany (c, brkterm))
	    continue;
    }
    broken[MAXARGS] = 0;

    return broken;
}


static  brkany (chr, strg)
register char   chr,
               *strg;
{
    register char  *sp;

    if (strg)
	for (sp = strg; *sp; sp++)
	    if (chr == *sp)
		return 1;
    return 0;
}


static char **copyip (p, q)
register char **p,
              **q;
{
    while (*p)
	*q++ = *p++;
    *q = 0;

    return q;
}
#endif
#endif /* MPOP */

/*  */

char *EHLOset (s)
char *s;
{
    int	    len = strlen (s);
    register char  *ep,
		 **ehlo;

    for (ehlo = EHLOkeys; ep = *ehlo; ehlo++)
	if (strncmp (ep, s, len) == 0) {
	    for (ep += len; *ep == ' '; ep++)
		continue;
	    return ep;
	}

    return 0;
}
