/* popsbr.c - POP client subroutines */
#ifndef	lint
static char ident[] = "@(#)$Id: popsbr.c,v 2.6 1993/08/26 18:25:52 jromine Exp $";
#endif	lint

#if defined(NNTP) && !defined(PSHSBR)
#undef  NNTP
#endif

/* LINTLIBRARY */

#include "../h/strings.h"
#ifdef NNTP			/* building pshsbr.o from popsbr.c */
#include "../h/nntp.h"
#endif /* NNTP */
#include <stdio.h>
#include <signal.h>

#ifndef	POPSERVICE
#define	POPSERVICE	"pop"
#endif

#define	NOTOK	(-1)
#define	OK	0
#define	DONE	1

#define	TRM	"."
#define	TRMLEN	(sizeof TRM - 1)

extern int  errno;
#ifndef	BSD44
extern int  sys_nerr;
extern char *sys_errlist[];
#endif

static int  poprint = 0;
static int  pophack = 0;

char    response[BUFSIZ];

static FILE *input;
static FILE *output;

#ifdef __STDC__
static int  traverse (int (*)(), const char*, char *, char *, char *, char *);
#define	targ_t	char *
#else
static int	traverse();
#define	targ_t	int
#endif

#if !defined(NNTP) && defined(MPOP)
#define	command	pop_command
#define	multiline pop_multiline
int	command(), multiline();
#else
static	int	command(), multiline();
#endif

static int	getline();
static putline();

#ifdef NNTP
#ifdef	BPOP	/* stupid */
static	int	xtnd_last = -1,
		xtnd_first = 0;
static char	xtnd_name[512];	/* INCREDIBLE HACK!! */
#endif
#endif /* NNTP */

/*  */

#ifndef	NNTP
#ifdef	APOP
#include "md5.c"

static char *pop_auth (user, pass)
char   *user,
       *pass;
{
    register char *cp,
		  *lp;
    register unsigned char *dp;
    unsigned char *ep,
		   digest[16];
    MD5_CTX mdContext;
    static char buffer[BUFSIZ];

    if ((cp = index (response, '<')) == NULL
	    || (lp = index (cp, '>')) == NULL) {
	(void) sprintf (buffer, "APOP not available: %s", response);
	(void) strcpy (response, buffer);
	return NULL;
    }

    *++lp = NULL;
    (void) sprintf (buffer, "%s%s", cp, pass);

    MD5Init (&mdContext);
    MD5Update (&mdContext, (unsigned char *) buffer,
	       (unsigned int) strlen (buffer));
    MD5Final (digest, &mdContext);

    (void) sprintf (cp = buffer, "%s ", user);
    cp += strlen (cp);
    for (ep = (dp = digest) + sizeof digest / sizeof digest[0];
	     dp < ep;
	     cp += 2)
	(void) sprintf (cp, "%02x", *dp++ & 0xff);
    *cp = NULL;

    return buffer;
}
#endif	/* APOP */
#endif	/* !NNTP */

/*  */

#if defined(RPOP) || defined(APOP)
int     pop_init (host, user, pass, snoop, rpop)
int     rpop;
#else
int     pop_init (host, user, pass, snoop)
#endif
char   *host,
       *user,
       *pass;
int	snoop;
{
#ifdef	APOP
    int     apop;
#else
#ifndef	RPOP	/* !APOP && !RPOP */
    int	    rpop = 0;
#endif
#endif
    int     fd1,
            fd2;
    char    buffer[BUFSIZ];

#ifdef	APOP
    if ((apop = rpop) < 0)
	rpop = 0;
#endif	/* APOP */

#ifndef NNTP
#ifndef	KPOP
    if ((fd1 = client (host, "tcp", POPSERVICE, rpop, response)) == NOTOK)
#else	/* KPOP */
    (void) sprintf (buffer, "%s/%s", POPSERVICE, "kpop");
    if ((fd1 = client (host, "tcp", buffer, rpop, response)) == NOTOK)
#endif
#else	/* NNTP */
    if ((fd1 = client (host, "tcp", "nntp", rpop, response)) == NOTOK)
#endif
	return NOTOK;

    if ((fd2 = dup (fd1)) == NOTOK) {
	(void) sprintf (response, "unable to dup connection descriptor: %s",
		errno > 0 && errno < sys_nerr ? sys_errlist[errno]
		: "unknown error");
	(void) close (fd1);
	return NOTOK;
    }
#ifndef NNTP
    if (pop_set (fd1, fd2, snoop) == NOTOK)
#else	/* NNTP */
    if (pop_set (fd1, fd2, snoop, (char *)0) == NOTOK)
#endif	/* NNTP */
	return NOTOK;

    (void) signal (SIGPIPE, SIG_IGN);

    switch (getline (response, sizeof response, input)) {
	case OK: 
	    if (poprint)
		fprintf (stderr, "<--- %s\n", response);
#ifndef	NNTP
	    if (*response == '+') {
#ifndef	KPOP
#ifdef	APOP
		if (apop < 0) {
		    char   *cp = pop_auth (user, pass);

		    if (cp && command ("APOP %s", cp) != NOTOK)
			return OK;
		}
		else
#endif	/* apop */
		if (command ("USER %s", user) != NOTOK
		    && command ("%s %s", rpop ? "RPOP" : (pophack++, "PASS"),
					pass) != NOTOK)
		return OK;
#else	/* KPOP */
		if (command ("USER %s", user) != NOTOK
		    && command ("PASS %s", pass) != NOTOK)
		return OK;
#endif
	    }
#else	/* NNTP */
	    if (*response < CHAR_ERR) {
		(void) command ("MODE READER");
		return OK;
	    }
#endif
	    (void) strcpy (buffer, response);
	    (void) command ("QUIT");
	    (void) strcpy (response, buffer);
				/* and fall */

	case NOTOK: 
	case DONE: 
	    if (poprint)	    
		fprintf (stderr, "%s\n", response);
	    (void) fclose (input);
	    (void) fclose (output);
	    return NOTOK;
    }
/* NOTREACHED */
}
/*  */

#ifndef NNTP
int	pop_set (in, out, snoop)
#else	/* NNTP */
int	pop_set (in, out, snoop, myname)
char   *myname;
#endif	/* NNTP */
int	in,
	out,
	snoop;
{
#ifdef NNTP
    if (myname && *myname)
	strcpy (xtnd_name, myname);	/* interface from bbc to msh */

#endif	/* NNTP */
    if ((input = fdopen (in, "r")) == NULL
	    || (output = fdopen (out, "w")) == NULL) {
	(void) strcpy (response, "fdopen failed on connection descriptor");
	if (input)
	    (void) fclose (input);
	else
	    (void) close (in);
	(void) close (out);
	return NOTOK;
    }

    poprint = snoop;

    return OK;
}


int	pop_fd (in, out)
char   *in,
       *out;
{
    (void) sprintf (in, "%d", fileno (input));
    (void) sprintf (out, "%d", fileno (output));
    return OK;
}

/*  */

int     pop_stat (nmsgs, nbytes)
int    *nmsgs,
       *nbytes;
{
#ifdef NNTP
    char **ap;
    extern char **brkstring();
#endif	/* NNTP */

#ifndef	NNTP
    if (command ("STAT") == NOTOK)
	return NOTOK;

    *nmsgs = *nbytes = 0;
    (void) sscanf (response, "+OK %d %d", nmsgs, nbytes);

#else	/* NNTP */
    if (xtnd_last < 0) { 	/* in msh, xtnd_name is set from myname */
	if (command("GROUP %s", xtnd_name) == NOTOK)
	    return NOTOK;

	ap = brkstring (response, " ", "\n"); /* "211 nart first last ggg" */
	xtnd_first = atoi (ap[2]);
	xtnd_last  = atoi (ap[3]);
    }

    /* nmsgs is not the real nart, but an incredible simuation */
    if (xtnd_last > 0)
	*nmsgs = xtnd_last - xtnd_first + 1;	/* because of holes... */
    else
	*nmsgs = 0;
    *nbytes = xtnd_first;	/* for subtracting offset in msh() */
#endif	/* NNTP */

    return OK;
}

#ifdef NNTP
int	pop_exists (action)
int	(*action) ();
{
#ifdef	XMSGS		/* hacked into NNTP 1.5 */
    if (traverse (action, "XMSGS %d-%d",
	    (targ_t)xtnd_first, (targ_t)xtnd_last, 0, 0) == OK)
	return OK;
#endif
    if (traverse (action, "LISTGROUP",	/* provided by INN 1.4 */
	    0, 0, 0, 0) == OK)
	return OK;
    return traverse (action, "XHDR NONAME %d-%d",
	    (targ_t)xtnd_first, (targ_t)xtnd_last, 0, 0);
}
#endif	/* NNTP */

#ifndef	BPOP
int     pop_list (msgno, nmsgs, msgs, bytes)
#else	BPOP
int     pop_list (msgno, nmsgs, msgs, bytes, ids)
int    *ids;
#endif	BPOP
int     msgno,
       *nmsgs,
       *msgs,
       *bytes;
{
    int     i;
#ifndef	BPOP
    int    *ids = NULL;
#endif

    if (msgno) {
#ifndef NNTP
	if (command ("LIST %d", msgno) == NOTOK)
	    return NOTOK;
	*msgs = *bytes = 0;
	if (ids) {
	    *ids = 0;
	    (void) sscanf (response, "+OK %d %d %d", msgs, bytes, ids);
	}
	else
	    (void) sscanf (response, "+OK %d %d", msgs, bytes);
#else	/* NNTP */
	*msgs = *bytes = 0;
	if (command ("STAT %d", msgno) == NOTOK) 
	    return NOTOK;
	if (ids) {
	    *ids = msgno;
	}
#endif	/* NNTP */
	return OK;
    }

#ifndef NNTP
    if (command ("LIST") == NOTOK)
	return NOTOK;

    for (i = 0; i < *nmsgs; i++)
	switch (multiline ()) {
	    case NOTOK: 
		return NOTOK;
	    case DONE: 
		*nmsgs = ++i;
		return OK;
	    case OK: 
		*msgs = *bytes = 0;
		if (ids) {
		    *ids = 0;
		    (void) sscanf (response, "%d %d %d",
			    msgs++, bytes++, ids++);
		}
		else
		    (void) sscanf (response, "%d %d", msgs++, bytes++);
		break;
	}
    for (;;)
	switch (multiline ()) {
	    case NOTOK: 
		return NOTOK;
	    case DONE: 
		return OK;
	    case OK: 
		break;
	}
#else	/* NNTP */
    return NOTOK;
#endif	/* NNTP */
}

/*  */

int     pop_retr (msgno, action)
int     msgno,
        (*action) ();
{
#ifndef NNTP
    return traverse (action, "RETR %d", (targ_t)msgno, 0, 0, 0);
#else	/* NNTP */
    return traverse (action, "ARTICLE %d", (targ_t)msgno, 0, 0, 0);
#endif	/* NNTP */
}


/* VARARGS2 */

static int  traverse (action, fmt, a, b, c, d)
int     (*action) ();
#ifdef __STDC__
const char   *fmt;
#else
char *fmt;
#endif
char   *a,
       *b,
       *c,
       *d;
{
    char    buffer[sizeof response];

    if (command (fmt, a, b, c, d) == NOTOK)
	return NOTOK;
    (void) strcpy (buffer, response);

    for (;;)
	switch (multiline ()) {
	    case NOTOK: 
		return NOTOK;

	    case DONE: 
		(void) strcpy (response, buffer);
		return OK;

	    case OK: 
		(*action) (response);
		break;
	}
}

/*  */

int     pop_dele (msgno)
int     msgno;
{
    return command ("DELE %d", msgno);
}


int     pop_noop () {
    return command ("NOOP");
}


#ifndef	NNTP
#ifdef	MPOP
int     pop_last () {
    return command ("LAST");
}
#endif	/* MPOP */
#endif	/* !NNTP */

int     pop_rset () {
    return command ("RSET");
}

/*  */

int     pop_top (msgno, lines, action)
int     msgno,
	lines,
        (*action) ();
{
#ifndef NNTP
    return traverse (action, "TOP %d %d", (targ_t)msgno, (targ_t)lines, 0, 0);
#else	/* NNTP */
    return traverse (action, "HEAD %d", (targ_t)msgno, 0, 0, 0);
#endif	/* NNTP */
}


#ifdef	BPOP
int	pop_xtnd (action, fmt, a, b, c, d)
int     (*action) ();
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    char buffer[BUFSIZ];
#ifdef NNTP
    extern char **brkstring();
    char  **ap;
#endif	/* NNTP */

#ifndef NNTP
    (void) sprintf (buffer, "XTND %s", fmt);
    return traverse (action, buffer, a, b, c, d);
#else	/* NNTP */
    sprintf (buffer, fmt, a, b, c, d);
    ap = brkstring (buffer, " ", "\n");	/* a hack, i know... */

    if (uleq(ap[0], "x-bboards")) {	/* XTND "X-BBOARDS group */
	/* most of these parameters are meaningless under NNTP. 
	 * bbc.c was modified to set AKA and LEADERS as appropriate,
	 * the rest are left blank.
	 */
	return OK;
    }
    if (uleq (ap[0], "archive") && ap[1]) {
	sprintf (xtnd_name, "%s", ap[1]);		/* save the name */
	xtnd_last = 0;
	xtnd_first = 1;		/* setup to fail in pop_stat */
	return OK;
    }
    if (uleq (ap[0], "bboards")) {

	if (ap[1]) {			/* XTND "BBOARDS group" */
	    sprintf (xtnd_name, "%s", ap[1]);		/* save the name */
	    if (command("GROUP %s", xtnd_name) == NOTOK)
		return NOTOK;

	    strcpy (buffer, response);	/* action must ignore extra args */
	    ap = brkstring (response, " ", "\n");/* "211 nart first last g" */
	    xtnd_first = atoi (ap[2]);
	    xtnd_last  = atoi (ap[3]);

	    (*action) (buffer);		
	    return OK;

	} else {		/* XTND "BBOARDS" */
	    return traverse (action, "LIST", a, b, c, d);
	}
    }
    return NOTOK;	/* unknown XTND command */
#endif	/* NNTP */
}
#endif	BPOP

/*  */

int     pop_quit () {
    int     i;

    i = command ("QUIT");
    (void) pop_done ();

    return i;
}


int     pop_done () {
    (void) fclose (input);
    (void) fclose (output);

    return OK;
}

/*  */

/* VARARGS1 */

#if defined(MPOP) && !defined(NNTP)
int	command (fmt, a, b, c, d)
#else
static int  command (fmt, a, b, c, d)
#endif
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    char   *cp,
	    buffer[BUFSIZ];

    (void) sprintf (buffer, fmt, a, b, c, d);
    if (poprint)
	if (pophack) {
	    if (cp = index (buffer, ' '))
		*cp = 0;
	    fprintf (stderr, "---> %s ********\n", buffer);
	    if (cp)
		*cp = ' ';
	    pophack = 0;
	}
	else
	    fprintf (stderr, "---> %s\n", buffer);

    if (putline (buffer, output) == NOTOK)
	return NOTOK;

    switch (getline (response, sizeof response, input)) {
	case OK: 
	    if (poprint)
		fprintf (stderr, "<--- %s\n", response);
#ifndef NNTP
	    return (*response == '+' ? OK : NOTOK);
#else	/* NNTP */
	    return (*response < CHAR_ERR ? OK : NOTOK);
#endif	/* NNTP */

	case NOTOK: 
	case DONE: 
	    if (poprint)	    
		fprintf (stderr, "%s\n", response);
	    return NOTOK;
    }
/* NOTREACHED */
}

#if	defined(MPOP) && !defined(NNTP)
int  multiline () {
#else
static int  multiline () {
#endif
    char    buffer[BUFSIZ + TRMLEN];

    if (getline (buffer, sizeof buffer, input) != OK)
	return NOTOK;
#ifdef	DEBUG
    if (poprint)
	fprintf (stderr, "<--- %s\n", response);
#endif	DEBUG
    if (strncmp (buffer, TRM, TRMLEN) == 0) {
	if (buffer[TRMLEN] == 0)
	    return DONE;
	else
	    (void) strcpy (response, buffer + TRMLEN);
    }
    else
	(void) strcpy (response, buffer);

    return OK;
}

/*  */

static int  getline (s, n, iop)
char   *s;
int     n;
FILE * iop;
{
    int     c;
    char   *p;

    p = s;
    while (--n > 0 && (c = fgetc (iop)) != EOF)
	if ((*p++ = c) == '\n')
	    break;
    if (ferror (iop) && c != EOF) {
	(void) strcpy (response, "error on connection");
	return NOTOK;
    }
    if (c == EOF && p == s) {
	(void) strcpy (response, "connection closed by foreign host");
	return DONE;
    }
    *p = 0;
    if (*--p == '\n')
	*p = 0;
    if (*--p == '\r')
	*p = 0;

    return OK;
}


static  putline (s, iop)
char   *s;
FILE * iop;
{
    (void) fprintf (iop, "%s\r\n", s);
    (void) fflush (iop);
    if (ferror (iop)) {
	(void) strcpy (response, "lost connection");
	return NOTOK;
    }

    return OK;
}
