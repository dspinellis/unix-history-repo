/* popsbr.c - POP client subroutines */

/* LINTLIBRARY */

#include "../h/strings.h"
#include <stdio.h>
#include <signal.h>


#define	NOTOK	(-1)
#define	OK	0
#define	DONE	1

#define	TRM	"."
#define	TRMLEN	(sizeof TRM - 1)

extern int  errno;
extern int  sys_nerr;
extern char *sys_errlist[];

static int  poprint = 0;
static int  pophack = 0;

char    response[BUFSIZ];

static FILE *input;
static FILE *output;

/*  */

#ifndef	RPOP
int     pop_init (host, user, pass, snoop)
#else	RPOP
int     pop_init (host, user, pass, snoop, rpop)
int     rpop;
#endif	RPOP
char   *host,
       *user,
       *pass;
int	snoop;
{
    int     fd1,
            fd2;
#ifndef	RPOP
    int	    rpop = 0;
#endif	RPOP
    char    buffer[BUFSIZ];

    if ((fd1 = client (host, "tcp", "pop", rpop, response)) == NOTOK)
	return NOTOK;

    if ((fd2 = dup (fd1)) == NOTOK) {
	(void) sprintf (response, "unable to dup connection descriptor: %s",
		errno > 0 && errno < sys_nerr ? sys_errlist[errno]
		: "unknown error");
	(void) close (fd1);
	return NOTOK;
    }
    if (pop_set (fd1, fd2, snoop) == NOTOK)
	return NOTOK;

    (void) signal (SIGPIPE, SIG_IGN);

    switch (getline (response, sizeof response, input)) {
	case OK: 
	    if (poprint)
		fprintf (stderr, "<--- %s\n", response);
	    if (*response == '+'
		    && command ("USER %s", user) != NOTOK
		    && command ("%s %s", rpop ? "RPOP" : (pophack++, "PASS"),
					pass) != NOTOK)
		return OK;
	    if (*response != '+') {
		(void) strcpy (buffer, response);
		(void) command ("QUIT");
		(void) strcpy (response, buffer);
	    }			/* fall */

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

int	pop_set (in, out, snoop)
int	in,
	out,
	snoop;
{
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
    if (command ("STAT") == NOTOK)
	return NOTOK;

    *nmsgs = *nbytes = 0;
    (void) sscanf (response, "+OK %d %d", nmsgs, nbytes);
    return OK;
}


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
#endif	not BPOP

    if (msgno) {
	if (command ("LIST %d", msgno) == NOTOK)
	    return NOTOK;

	*msgs = *bytes = 0;
	if (ids) {
	    *ids = 0;
	    (void) sscanf (response, "+OK %d %d %d", msgs, bytes, ids);
	}
	else
	    (void) sscanf (response, "+OK %d %d", msgs, bytes);
	return OK;
    }

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
}

/*  */

int     pop_retr (msgno, action)
int     msgno,
        (*action) ();
{
    return traverse (action, "RETR %d", msgno);
}


/* VARARGS2 */

static int  traverse (action, fmt, a, b, c, d)
int     (*action) ();
char   *fmt,
       *a,
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


int     pop_rset () {
    return command ("RSET");
}

/*  */

int     pop_top (msgno, lines, action)
int     msgno,
	lines,
        (*action) ();
{
    return traverse (action, "TOP %d %d", msgno, lines);
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

    (void) sprintf (buffer, "XTND %s", fmt);
    return traverse (action, buffer, a, b, c, d);
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

static int  command (fmt, a, b, c, d)
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
		*cp = NULL;
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
	    return (*response == '+' ? OK : NOTOK);

	case NOTOK: 
	case DONE: 
	    if (poprint)	    
		fprintf (stderr, "%s\n", response);
	    return NOTOK;
    }
/* NOTREACHED */
}

static int  multiline () {
    char    buffer[BUFSIZ + TRMLEN];

    if (getline (buffer, sizeof buffer, input) != OK)
	return NOTOK;
    if (strncmp (buffer, TRM, TRMLEN) == 0) {
	if (buffer[TRMLEN] == NULL)
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
    if (ferror (iop)) {
	(void) strcpy (response, "error on connection");
	return NOTOK;
    }
    if (c == EOF && p == s) {
	(void) strcpy (response, "connection closed by foreign host");
	return DONE;
    }
    *p = NULL;
    if (*--p == '\n')
	*p = NULL;
    if (*--p == '\r')
	*p = NULL;

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
