/* logger.c - system logging routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/logger.c,v 7.6 91/02/22 09:15:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/logger.c,v 7.6 91/02/22 09:15:32 mrose Interim $
 *
 *
 * $Log:	logger.c,v $
 * Revision 7.6  91/02/22  09:15:32  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/11/21  11:29:50  mrose
 * sun
 * 
 * Revision 7.4  90/10/23  20:40:43  mrose
 * update
 * 
 * Revision 7.3  90/08/14  14:25:18  mrose
 * CHKINT
 * 
 * Revision 7.2  90/04/23  00:08:12  mrose
 * touch-up
 * 
 * Revision 7.1  90/02/19  13:07:17  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:23:17  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include <varargs.h>
#include "general.h"
#include "manifest.h"
#include "logger.h"
#include "tailor.h"

#ifdef	NULL
#undef	NULL
#endif
#include <sys/param.h>
#ifndef	NULL
#define	NULL	0
#endif
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include <sys/stat.h>

#ifndef	BSD42
#include <time.h>
#else
#ifndef	timerisset
#include <sys/time.h>
#endif
#endif

#ifndef	SYS5
#include <syslog.h>
#endif

/*  */

struct ll_private {
    int	    ll_checks;
#define	CHKINT	15		/* call ll_check 1 in every 15 uses... */
};

static struct ll_private *llp = NULL;
static IFP _ll_header_routine = ll_defmhdr;


long	lseek (), time ();

/*  */

int	ll_open (lp)
register LLog *lp;
{
    int	    mask,
	    mode;
    char   *bp,
	    buffer[BUFSIZ];
    struct stat st;

    if (llp == NULL
	    && (llp = (struct ll_private *)
			calloc ((unsigned int) getdtablesize (),
				sizeof *llp)) == NULL)
	goto you_lose;

    if (lp -> ll_file == NULLCP
	    || *lp -> ll_file == NULL) {
you_lose: ;
	(void) ll_close (lp);
	lp -> ll_stat |= LLOGERR;
	return NOTOK;
    }

    lp -> ll_stat &= ~LLOGERR;

    if (lp -> ll_fd != NOTOK)
	return OK;

    if (strcmp (lp -> ll_file, "-") == 0) {
	lp -> ll_stat |= LLOGTTY;
	return OK;
    }

    (void) sprintf (bp = buffer, _isodefile (isodelogpath, lp -> ll_file),
		    getpid ());

    mode = O_WRONLY | O_APPEND;
    if (stat (bp, &st) == NOTOK && (lp -> ll_stat & LLOGCRT))
	mode |= O_CREAT;

    mask = umask (~0666);
    lp -> ll_fd = open (bp, mode, 0666);
    (void) umask (mask);

    if (ll_check (lp) == NOTOK)
	return (NOTOK);
    if (lp -> ll_fd != NOTOK)
	llp[lp -> ll_fd].ll_checks = CHKINT;

    return (lp -> ll_fd != NOTOK ? OK : NOTOK);
}

/*  */

int	ll_close (lp)
register LLog *lp;
{
    int	    status;
    
    if (lp -> ll_fd == NOTOK)
	return OK;

    status = close (lp -> ll_fd);
    lp -> ll_fd = NOTOK;

    return status;
}

/*  */

#ifndef	lint
int	ll_log (va_alist)
va_dcl
{
    int	    event,
	    result;
    LLog   *lp;
    va_list ap;

    va_start (ap);

    lp = va_arg (ap, LLog *);
    event = va_arg (ap, int);

    result = _ll_log (lp, event, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	ll_log (lp, event, what, fmt)
LLog   *lp;
int	event;
char   *what,
       *fmt;
{
    return ll_log (lp, event, what, fmt);
}
#endif

/*  */

int	_ll_log (lp, event, ap)	/* what, fmt, args ... */
register LLog *lp;
int	event;
va_list	ap;
{
    int	    cc,
	    status;
    register char *bp;
    char   *what,
	    buffer[BUFSIZ];

    if (!(lp -> ll_events & event))
	return OK;

    bp = buffer;

    /* Create header */
    (*_ll_header_routine)(bp, lp -> ll_hdr, lp -> ll_dhdr);

    bp += strlen (bp);

    what = va_arg (ap, char *);

    _asprintf (bp, what, ap);

#ifndef	SYS5
    if (lp -> ll_syslog & event) {
	int	priority;

	switch (event) {
	    case LLOG_FATAL:
	        priority = LOG_ERR;
		break;

	    case LLOG_EXCEPTIONS:
		priority = LOG_WARNING;
		break;

	    case LLOG_NOTICE:
	        priority = LOG_INFO;
		break;

	    case LLOG_PDUS:
	    case LLOG_TRACE:
	    case LLOG_DEBUG:
	        priority = LOG_DEBUG;
		break;

	    default:
		priority = LOG_NOTICE;
		break;
	}

	(void) syslog (priority, "%s", buffer + 13);

	if (lp -> ll_stat & LLOGCLS)
	    (void) closelog ();
    }
#endif

    if (!(lp -> ll_stat & LLOGTTY)
	    && lp -> ll_fd == NOTOK
	    && strcmp (lp -> ll_file, "-") == 0)
	lp -> ll_stat |= LLOGTTY;

    if (lp -> ll_stat & LLOGTTY) {
	(void) fflush (stdout);

	if (lp -> ll_fd != NOTOK)
	    (void) fprintf (stderr, "LOGGING: ");
	(void) fputs (bp, stderr);
	(void) fputc ('\n', stderr);
	(void) fflush (stderr);
    }
    bp += strlen (bp);

    if (lp -> ll_fd == NOTOK) {
	if ((lp -> ll_stat & (LLOGERR | LLOGTTY)) == (LLOGERR | LLOGTTY))
	    return OK;
	if (ll_open (lp) == NOTOK)
	    return NOTOK;
    }
    else
	if ((!llp || llp[lp -> ll_fd].ll_checks-- < 0)
		&& ll_check (lp) == NOTOK)
	    return NOTOK;

    *bp++ = '\n', *bp = NULL;
    cc = bp - buffer;

    if ((status = write (lp -> ll_fd, buffer, cc)) != cc) {
	if (status == NOTOK) {
	    (void) ll_close (lp);
error: ;
	    lp -> ll_stat |= LLOGERR;
	    return NOTOK;
	}

	status = NOTOK;
    }
    else
	status = OK;

    if ((lp -> ll_stat & LLOGCLS) && ll_close (lp) == NOTOK)
	goto error;

    return status;
}

/*  */

void	ll_hdinit (lp, prefix)
register LLog *lp;
char   *prefix;
{
    register char  *cp,
		   *up;
    char    buffer[BUFSIZ],
	    user[10];

    if (prefix == NULLCP) {
	if ((lp -> ll_stat & LLOGHDR) && strlen (lp -> ll_hdr) == 25)
	    (cp = lp -> ll_hdr)[8] = NULL;
	else
	    cp = "unknown";
    }
    else {
	if ((cp = rindex (prefix, '/')))
	    cp++;
	if (cp == NULL || *cp == NULL)
	    cp = prefix;
    }

    if ((up = getenv ("USER")) == NULLCP
	    && (up = getenv ("LOGNAME")) == NULLCP) {
	(void) sprintf (user, "#%d", getuid ());
	up = user;
    }
    (void) sprintf (buffer, "%-8.8s %05d (%-8.8s)",
		    cp, getpid () % 100000, up);

    if (lp -> ll_stat & LLOGHDR)
	free (lp -> ll_hdr);
    lp -> ll_stat &= ~LLOGHDR;

    if ((lp -> ll_hdr = malloc ((unsigned) (strlen (buffer) + 1))) == NULLCP)
	return;

    (void) strcpy (lp -> ll_hdr, buffer);
    lp -> ll_stat |= LLOGHDR;
}

/*  */

void	ll_dbinit (lp, prefix)
register LLog *lp;
char   *prefix;
{
    register char  *cp;
    char    buffer[BUFSIZ];

    ll_hdinit (lp, prefix);

    if (prefix) {
	if ((cp = rindex (prefix, '/')))
	    cp++;
	if (cp == NULL || *cp == NULL)
	    cp = prefix;

	(void) sprintf (buffer, "./%s.log", cp);

	if ((lp -> ll_file = malloc ((unsigned) (strlen (buffer) + 1)))
	        == NULLCP)
	    return;

	(void) strcpy (lp -> ll_file, buffer);
    }

    lp -> ll_events |= LLOG_ALL;
    lp -> ll_stat |= LLOGTTY;
}

/*  */

#ifndef	lint
int	ll_printf (va_alist)
va_dcl
{
    int	    result;
    LLog    *lp;
    va_list ap;

    va_start (ap);

    lp = va_arg (ap, LLog *);

    result = _ll_printf (lp, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS2 */

int	ll_printf (lp, fmt)
LLog   *lp;
char   *fmt;
{
    return ll_printf (lp, fmt);
}
#endif

/*  */

#ifndef	lint
static
#endif
int  _ll_printf (lp, ap)		/* fmt, args ... */
register LLog *lp;
va_list	ap;
{
    int	    cc,
	    status;
    register char   *bp;
    char     buffer[BUFSIZ];
    char    *fmt;
    va_list fp = ap;

    fmt = va_arg (fp, char *);
    if (strcmp (fmt, "%s") != 0) {
	bp = buffer;
	_asprintf (bp, NULLCP, ap);
    }
    else {
	bp = NULL;
	fmt = va_arg (fp, char *);
    }

    if (!(lp -> ll_stat & LLOGTTY)
	    && lp -> ll_fd == NOTOK
	    && strcmp (lp -> ll_file, "-") == 0)
	lp -> ll_stat |= LLOGTTY;

    if (lp -> ll_stat & LLOGTTY) {
	(void) fflush (stdout);

	if (bp)
	    (void) fputs (bp, stderr);
	else
	    (void) fputs (fmt, stderr);
	(void) fflush (stderr);
    }
    if (bp)
	bp += strlen (bp);

    if (lp -> ll_fd == NOTOK) {
	if ((lp -> ll_stat & (LLOGERR | LLOGTTY)) == (LLOGERR | LLOGTTY))
	    return OK;
	if (ll_open (lp) == NOTOK)
	    return NOTOK;
    }
    else
	if ((!llp || llp[lp -> ll_fd].ll_checks-- < 0)
		&& ll_check (lp) == NOTOK)
	    return NOTOK;

    if (bp)
	cc = bp - buffer;
    else
	cc = strlen (fmt);

    if ((status = write (lp -> ll_fd, bp ? buffer : fmt, cc)) != cc) {
	if (status == NOTOK) {
	    (void) ll_close (lp);
	    lp -> ll_stat |= LLOGERR;
	    return NOTOK;
	}

	status = NOTOK;
    }
    else
	status = OK;

    return status;
}

/*  */

int	ll_sync (lp)
register LLog *lp;
{
    if (lp -> ll_stat & LLOGCLS)
	return ll_close (lp);

    return OK;
}

/*  */

#ifndef	lint
char   *ll_preset (va_alist)
va_dcl
{
    va_list ap;
    static char buffer[BUFSIZ];

    va_start (ap);

    _asprintf (buffer, NULLCP, ap);

    va_end (ap);

    return buffer;
}
#else
/* VARARGS1 */

char   *ll_preset (fmt)
char   *fmt;
{
    return ll_preset (fmt);
}
#endif

/*  */

int	ll_check (lp)
register LLog *lp;
{
#ifndef	BSD42
    int	    fd;
    char    buffer[BUFSIZ];
#endif
    long    size;
    struct stat st;

    if ((size = lp -> ll_msize) <= 0)
	return OK;

    if (llp && lp -> ll_fd != NOTOK)
	llp[lp -> ll_fd].ll_checks = CHKINT;
    if (lp -> ll_fd == NOTOK
	    || (fstat (lp -> ll_fd, &st) != NOTOK
		    && st.st_size < (size <<= 10)))
	return OK;

    if (!(lp -> ll_stat & LLOGZER)) {
	(void) ll_close (lp);

#ifndef	BSD42
error: ;
#endif
	lp -> ll_stat |= LLOGERR;
	return NOTOK;
    }

#ifdef	BSD42
#ifdef	SUNOS4
    (void) ftruncate (lp -> ll_fd, (off_t) 0);
#else
    (void) ftruncate (lp -> ll_fd, 0);
#endif
    (void) lseek (lp -> ll_fd, 0L, 0);
    return OK;
#else
    (void) sprintf (buffer, _isodefile (isodelogpath, lp -> ll_file),
		    getpid ());
    if ((fd = open (buffer, O_WRONLY | O_APPEND | O_TRUNC)) == NOTOK)
	goto error;
    (void) close (fd);
    return OK;
#endif
}

/*  */

/*
 * ll_defmhdr - Default "make header" routine.
 */
int	ll_defmhdr(bufferp, headerp, dheaderp)
char	*bufferp;		/* Buffer pointer */
char	*headerp;		/* Static header string */
char	*dheaderp;		/* Dynamic header string */
{
    long    clock;
    register struct tm *tm;

    (void) time (&clock);
    tm = localtime (&clock);

    (void) sprintf (bufferp, "%2d/%2d %2d:%02d:%02d %s %s ",
		    tm -> tm_mon + 1, tm -> tm_mday,
		    tm -> tm_hour, tm -> tm_min, tm -> tm_sec,
		    headerp ? headerp : "",
		    dheaderp ? dheaderp : "");
}

/*  */

/*
 * ll_setmhdr - Set "make header" routine, overriding default.
 */
IFP	ll_setmhdr (make_header_routine) 
IFP	make_header_routine;
{
    IFP result = _ll_header_routine;

    _ll_header_routine = make_header_routine;

    return result;

}
