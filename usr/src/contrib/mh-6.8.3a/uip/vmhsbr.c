/* vmhsbr.c - routines to help vmh along */
#ifndef	lint
static char ident[] = "@(#)$Id: vmhsbr.c,v 1.11 1993/08/25 17:29:53 jromine Exp $";
#endif	/* lint */

/* TODO (for vrsn 2):
	INI: include width of windows
 */

#include "../h/mh.h"
#include "../h/vmhsbr.h"
#include <stdio.h>

/*  */

static char *types[] = {
    "OK",
    "INI", "ACK", "ERR", "CMD", "QRY", "TTY", "WIN", "DATA", "EOF", "FIN",
    "XXX", NULL
};

static	FILE *fp = NULL;

static	int PEERrfd = NOTOK;
static	int PEERwfd = NOTOK;


extern int  errno;
#ifndef	BSD44
extern int  sys_nerr;
extern char *sys_errlist[];
#endif

static int	rclose();
/*  */

int	rcinit (rfd, wfd)
int	rfd,
	wfd;
{
    char   *cp,
            buffer[BUFSIZ];

    PEERrfd = rfd;
    PEERwfd = wfd;

    if ((cp = getenv ("MHVDEBUG")) && *cp) {
	(void) sprintf (buffer, "%s.out", invo_name);
	if (fp = fopen (buffer, "w")) {
	  (void) fseek (fp, 0L, 2);
	  fprintf (fp, "%d: rcinit (%d, %d)\n", getpid (), rfd, wfd);
	  (void) fflush (fp);
	}
    }

    return OK;
}


int     rcdone () {
    if (PEERrfd != NOTOK)
	(void) close (PEERrfd);
    if (PEERwfd != NOTOK)
	(void) close (PEERwfd);

    if (fp) {
	(void) fclose (fp);
	fp = NULL;
    }
    return OK;
}

/*  */

int	rc2rc (code, len, data, rc)
char	code;
int	len;
char   *data;
struct record *rc;
{
    if (rc2peer (code, len, data) == NOTOK)
	return NOTOK;

    return peer2rc (rc);
}


int	str2rc (code, str, rc)
char	code;
char   *str;
struct record *rc;
{
    return rc2rc (code, str ? strlen (str) : 0, str, rc);
}

/*  */

int	peer2rc (rc)
register struct	record *rc;
{
    if (rc -> rc_data)
	free (rc -> rc_data);

    if (read (PEERrfd, (char *) rc_head (rc), RHSIZE (rc)) != RHSIZE (rc))
	return rclose (rc, "read from peer lost(1)");
    if (rc -> rc_len) {
	if ((rc -> rc_data = malloc ((unsigned) rc -> rc_len + 1)) == NULL)
	    return rclose (rc, "malloc of %d lost", rc -> rc_len + 1);
	if (read (PEERrfd, rc -> rc_data, rc -> rc_len) != rc -> rc_len)
	    return rclose (rc, "read from peer lost(2)");
	rc -> rc_data[rc -> rc_len] = 0;
    }
    else
	rc -> rc_data = NULL;

    if (fp) {
	(void) fseek (fp, 0L, 2);
	fprintf (fp, "%d: <--- %s %d: \"%*.*s\"\n", getpid (),
		types[rc -> rc_type], rc -> rc_len,
		rc -> rc_len, rc -> rc_len, rc -> rc_data);
	(void) fflush (fp);
    }

    return rc -> rc_type;
}

/*  */

int	rc2peer (code, len, data)
char	code;
int	len;
char   *data;
{
    struct record   rcs;
    register struct record *rc = &rcs;

    rc -> rc_type = code;
    rc -> rc_len = len;

    if (fp) {
	(void) fseek (fp, 0L, 2);
	fprintf (fp, "%d: ---> %s %d: \"%*.*s\"\n", getpid (),
		types[rc -> rc_type], rc -> rc_len,
		rc -> rc_len, rc -> rc_len, data);
	(void) fflush (fp);
    }

    if (write (PEERwfd, (char *) rc_head (rc), RHSIZE (rc)) != RHSIZE (rc))
	return rclose (rc, "write to peer lost(1)");

    if (rc -> rc_len)
	if (write (PEERwfd, data, rc -> rc_len) != rc -> rc_len)
	    return rclose (rc, "write to peer lost(2)");

    return OK;
}

/*  */

int	str2peer (code, str)
char	code;
char   *str;
{
    return rc2peer (code, str ? strlen (str) : 0, str);
}


/* VARARGS2 */

int	fmt2peer (code, fmt, a, b, c, d, e, f)
char	code;
char   *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    return err2peer (code, NULLCP, fmt, a, b, c, d, e, f);
}

/*  */

/* VARARGS3 */

int	err2peer (code, what, fmt, a, b, c, d, e, f)
char	code;
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    int     eindex = errno;
    register char  *bp;
    char    buffer[BUFSIZ * 2];

    (void) sprintf (buffer, fmt, a, b, c, d, e, f);
    bp = buffer + strlen (buffer);
    if (what) {
	if (*what) {
	    (void) sprintf (bp, " %s: ", what);
	    bp += strlen (bp);
	}
	if (eindex > 0 && eindex < sys_nerr)
	    (void) strcpy (bp, sys_errlist[eindex]);
	else
	    (void) sprintf (bp, "Error %d", eindex);
	bp += strlen (bp);
    }

    return rc2peer (code, bp - buffer, buffer);
}

/*  */

/* VARARGS2 */

static	int	rclose (rc, fmt, a, b, c, d, e, f)
register struct record *rc;
char   *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    static char buffer[BUFSIZ * 2];

    (void) sprintf (buffer, fmt, a, b, c, d, e, f);

    rc -> rc_len = strlen (rc -> rc_data = getcpy (buffer));
    return (rc -> rc_type = RC_XXX);
}
