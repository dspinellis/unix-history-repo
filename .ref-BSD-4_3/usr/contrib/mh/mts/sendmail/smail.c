/* smail.c - MH interface to SendMail/SMTP */

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
#endif	BSD42

#ifndef	BSD42
#undef	SMTP
#endif	not BSD42
#ifdef	SMTP
#undef	SENDMAIL
#endif	SMTP


#include "../h/strings.h"
#include <stdio.h>
#include "smail.h"
#include "../zotnet/mts.h"
#include <ctype.h>
#include <signal.h>

#define	NOTOK	(-1)
#define	OK	0
#define	DONE	1

#define	TRUE	1
#define	FALSE	0

#define	NBITS	((sizeof (int)) * 8)

#define	min(a,b)	((a) < (b) ? (a) : (b))


#define	SM_OPEN	 30
#define	SM_HELO	 20
#define	SM_RSET	 15
#define	SM_MAIL	 40
#define	SM_RCPT	120
#define	SM_DATA	 20
#define	SM_TEXT	120
#define	SM_DOT	120
#define	SM_QUIT	 20
#define	SM_CLOS	 10

/*  */

int	alrmser ();

static int  sm_addrs = 0;
static int  sm_alarmed = 0;
#ifndef	SMTP
static int  sm_child = NOTOK;
#endif	not SMTP
static int  sm_debug = 0;
static int  sm_nl = TRUE;
static int  sm_verbose = 0;

static  FILE * sm_rfp = NULL;
static  FILE * sm_wfp = NULL;

static char *sm_noreply = "No reply text given";
static char *sm_moreply = "; ";

struct smtp sm_reply;		/* global... */


char   *r1bindex ();

/*  */

#ifndef	SMTP

/* ARGSUSED */

int     sm_init (client, server, watch, verbose, debug)
register char   *client;
char   *server;
register int     watch,
 	         verbose,
		 debug;
{
    register int    i,
                    result,
                    vecp;
    int     pdi[2],
            pdo[2];
    char   *vec[15];

    if (watch)
	verbose = TRUE;
    sm_verbose = verbose;
    sm_debug = debug;
    if (sm_rfp != NULL && sm_wfp != NULL)
	return RP_OK;

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
	    vec[vecp++] = watch ? "-odi" : "-odb";
	    vec[vecp++] = "-oem";
	    vec[vecp++] = "-om";
#ifndef	RAND
	    if (verbose)
		vec[vecp++] = "-ov";
#endif	not RAND
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
	    if (client)
		switch (smtalk (SM_HELO, "HELO %s", client)) {
		    case 250: 
			break;

		    default: 
			(void) sm_end (NOTOK);
			return RP_RPLY;
		}
	    return RP_OK;
    }
}
#else	SMTP

/*  */

int     sm_init (client, server, watch, verbose, debug)
register char   *client,
	        *server;
register int     watch,
  	         verbose,
		 debug;
{
    register int    result,
                    sd1,
                    sd2;

    if (watch)
	verbose = TRUE;
    sm_verbose = verbose;
    sm_debug = debug;
    if (sm_rfp != NULL && sm_wfp != NULL)
	return RP_OK;
#ifndef	SENDMTS
    if (client == NULL || *client == NULL)
	client = LocalName ();
#endif	not SENDMTS

    if ((sd1 = rclient (server, "tcp", "smtp")) == NOTOK)
	return RP_BHST;
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
    if (client && *client)
	switch (smtalk (SM_HELO, "HELO %s", client)) {
	    case 250: 
		break;

	    default: 
		(void) sm_end (NOTOK);
		return RP_RPLY;
	}

    return RP_OK;
}


static int rclient (server, protocol, service)
char   *server,
       *protocol,
       *service;
{
    int     sd;
    char    response[BUFSIZ];

    if ((sd = client (server, protocol, service, FALSE, response)) != NOTOK)
	return sd;

    (void) sm_ierror ("%s", response);
    return NOTOK;
}
#endif	SMTP

/*  */

int     sm_winit (mode, from)
register int	mode;
register char   *from;
{
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
#endif	BERK

int     sm_wadr (mbox, host, path)
register char   *mbox;
#ifndef	BERK
register
#endif	not BERK
	 char   *host,
		*path;
{
#ifndef	BERK
    switch (smtalk (SM_RCPT, host && *host ? "RCPT TO:<%s%s@%s>"
					   : "RCPT TO:<%s%s>",
			     path ? path : "", mbox, host)) {
#else	BERK
    switch (smtalk (SM_RCPT, "RCPT TO:%s", mbox)) {
#endif	BERK
	case 250: 
	case 251: 
	    sm_addrs++;
	    return RP_OK;

	case 421: 
	case 450: 
	case 451: 
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

	case 421: 
	case 451: 
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
#endif	not SMTP
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
#else	SMTP
	    (void) smtalk (SM_QUIT, "QUIT");
#endif	not SMTP
	    if (type == NOTOK) {
		sm_reply.code = sm_note.code;
		(void) strncpy (sm_reply.text, sm_note.text,
			sm_reply.length = sm_note.length);
	    }
	    break;
    }
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
#else	SMTP
    status = 0;
#endif	SMTP
    sm_rfp = sm_wfp = NULL;

    return (status ? RP_BHST : RP_OK);
}

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
    fputs ("\r\n", sm_wfp);
    (void) fflush (sm_wfp);

    return (ferror (sm_wfp) ? sm_werror () : OK);
}

/*  */

static int  sm_wstream (buffer, len)
register char   *buffer;
register int     len;
{
    register char  *bp;
    static char lc = NULL;

    if (sm_wfp == NULL)
	return sm_werror ();

    if (buffer == NULL && len == 0) {
	if (lc != '\n')
	    fputs ("\r\n", sm_wfp);
	lc = NULL;
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

static int  sm_werror () {
    sm_reply.length =
#ifdef	SMTP
	strlen (strcpy (sm_reply.text, sm_wfp == NULL ? "no socket opened"
	    : sm_alarmed ? "write to socket timed out"
	    : "error writing to socket"));
#else	not SMTP
	strlen (strcpy (sm_reply.text, sm_wfp == NULL ? "no pipe opened"
	    : sm_alarmed ? "write to pipe timed out"
	    : "error writing to pipe"));
#endif	not SMTP

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
    char    buffer[BUFSIZ];

again: ;

    sm_reply.text[sm_reply.length = 0] = NULL;

    rp = sm_reply.text, rc = sizeof sm_reply.text - 1;
    for (more = FALSE; sm_rrecord (bp = buffer, &bc) != NOTOK;) {
	if (sm_debug) {
	    printf ("<= %s\n", buffer);
	    (void) fflush (stdout);
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

    buffer[*len = 0] = NULL;

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
    buffer[*len - 1] = NULL;

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
#else	not SMTP
	strlen (strcpy (sm_reply.text, sm_rfp == NULL ? "no pipe opened"
	    : sm_alarmed ? "read from pipe timed out"
	    : feof (sm_rfp) ? "premature end-of-file on pipe"
	    : "error reading from pipe"));
#endif	not SMTP

    return (sm_reply.code = NOTOK);
}

/*  */

/* ARGSUSED */

static	int alrmser (i)
int     i;
{
#ifndef	BSD42
    signal (SIGALRM, alrmser);
#endif	BSD42
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
