/* lppd.c - lpp listen and dispatch daemon  */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/support/RCS/lppd.c,v 7.6 91/02/22 09:46:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/support/RCS/lppd.c,v 7.6 91/02/22 09:46:51 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	lppd.c,v $
 * Revision 7.6  91/02/22  09:46:51  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/19  09:16:08  mrose
 * touch-up
 * 
 * Revision 7.4  90/12/11  10:52:26  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/10/29  18:37:23  mrose
 * updates
 * 
 * Revision 7.2  90/07/09  14:50:54  mrose
 * sync
 * 
 * Revision 7.1  90/02/19  13:09:50  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:27:39  mrose
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


#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "manifest.h"
#include <sys/ioctl.h>
#include <sys/stat.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#ifndef	X_OK
#define	X_OK	1
#endif
#include "psap.h"
#include "tsap.h"
#include "isoservent.h"
#include "logger.h"
#include "tailor.h"

/*  */

static int  debug = 0;
static int  nbits = FD_SETSIZE;

static LLog _pgm_log = {
    "lppd.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;

static char *myname = "lppd";
static char myhost[BUFSIZ];


#define	NTADDRS		FD_SETSIZE

static struct TSAPaddr *tz;
static struct TSAPaddr  tas[NTADDRS];


struct dispatch {
    char       *dp_entity;

    u_short	dp_port;
};

static struct dispatch *dz;
static struct dispatch  dps[NTADDRS];


void	adios (), advise ();
void	ts_advise ();


extern int  errno;

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int	    listening,
	    vecp;
    char   *vec[4];
    register struct NSAPaddr  *na;
    register struct TSAPaddr  *ta;
    register struct dispatch  *dp;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    arginit (argv);
    envinit ();

    listening = 0;

    for (ta = tas, dp = dps; ta < tz; ta++, dp++) {
	na = ta -> ta_addrs;
	if (na -> na_stack != NA_TCP)
	    adios (NULLCP, "unexpected network type 0x%x", na -> na_stack);
	if (na -> na_tset != NA_TSET_TCP)
	    adios (NULLCP, "unexpected transport base 0x%x", na -> na_tset);

	advise (LLOG_NOTICE, NULLCP, "listening on %s for \"%s\"",
		taddr2str (ta), dp -> dp_entity);

	if (TNetListen (ta, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetListen failed");
	    _exit (1);
	}

	listening++;
    }

    if (!listening)
	adios (NULLCP, "no network services selected");

    for (ta = tas;;) {
	if (TNetAcceptAux (&vecp, vec, NULLIP, ta, 0, NULLFD, NULLFD, NULLFD,
			   NOTOK, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetAccept failed");
	    continue;
	}

	if (vecp <= 0)
	    continue;

	if (debug)
	    break;

	switch (TNetFork (vecp, vec, td)) {
	    case OK:
	        ll_hdinit (pgm_log, myname);
		break;

	    case NOTOK:
		ts_advise (td, LLOG_EXCEPTIONS, "TNetFork failed");
	    default:
		continue;
	}
	break;
    }

    lppd (vecp, vec, ta);
}

/*  */

/* ARGSUSED */

static int  lppd (vecp, vec, ta)
int	vecp;
char  **vec;
struct TSAPaddr *ta;
{
    register u_short port = ta -> ta_addrs[0].na_port;
    register struct dispatch *dp;
    register struct isoservent *is;

    for (dp = dps; dp < dz; dp++)
	if (dp -> dp_port == port)
	    break;
    if (dp >= dz)
	adios (NULLCP, "unable to find service associated with local port %d",
		ntohs (port));

    if ((is = getisoserventbyname (dp -> dp_entity, "lpp")) == NULL)
	adios (NULLCP, "unable to find program associated with service %s",
	       dp -> dp_entity);
    
    *is -> is_tail++ = vec[1];
    *is -> is_tail++ = vec[2];
    *is -> is_tail = NULL;

    advise (LLOG_NOTICE, NULLCP, "exec \"%s\" for \"%s\"", *is -> is_vec,
	    dp -> dp_entity);

    (void) execv (*is -> is_vec, is -> is_vec);

    adios (*is -> is_vec, "unable to exec");
}

/*  */

static void  ts_advise (td, code, event)
register struct TSAPdisconnect *td;
int	code;
char   *event;
{
    char    buffer[BUFSIZ];

    if (td -> td_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		TErrString (td -> td_reason),
		td -> td_cc, td -> td_cc, td -> td_data);
    else
	(void) sprintf (buffer, "[%s]", TErrString (td -> td_reason));

    advise (code, NULLCP, "%s: %s", event, buffer);
}

/*  */

static	arginit (vec)
char	**vec;
{
    register int    n;
    register char  *ap;
    struct stat st;
    register struct isoservent *is;
    register struct PSAPaddr *pa;
    register struct NSAPaddr *na;
    AEI	    aei;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 0);
    ll_hdinit (pgm_log, myname);

    if (getuid () == 0
	    && stat (ap = isodefile ("isoservices", 0), &st) != NOTOK
	    && st.st_uid != 0)
	adios (NULLCP, "%s not owned by root", ap);

    (void) strcpy (myhost, TLocalHostName ());

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    continue;

		case 's':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -s service-designator");
		    (void) strcpy (myhost, ap);
		    continue;
		
		default: 
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", myname);
    }

    bzero ((char *) tas, sizeof tas);
    tz = tas;

    bzero ((char *) dps, sizeof dps);
    dz = dps;

    (void) setisoservent (0);
    while (is = getisoservent ())
	if (strcmp (is -> is_provider, "lpp") == 0
		&& access (*is -> is_vec, X_OK) != NOTOK) {
	    if ((aei = str2aei (myhost, is -> is_entity)) == NULLAEI)
		continue;
	    if ((pa = aei2addr (aei)) == NULLPA)
		adios (NULLCP, "address translation failed on %s-%s",
		       myhost, is -> is_entity);

	    for (na = pa -> pa_addr.sa_addr.ta_addrs,
			n = pa -> pa_addr.sa_addr.ta_naddr;
		    n > 0;
		    na++, n--) {
		if (na -> na_stack != NA_TCP)
		    continue;
		if (na -> na_tset == 0)
		    na -> na_tset = NA_TSET_TCP;
		else
		    if (na -> na_tset != NA_TSET_TCP)
			continue;
		if (na -> na_port == 0)
		    continue;

		if (tz >= tas + NTADDRS) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "too many services, starting with %s",
			    is -> is_entity);
		    goto no_more;
		}

		bcopy ((char *) na, (char *) tz -> ta_addrs, sizeof *na);
		tz -> ta_naddr = 1;
		tz++;

		if ((dz -> dp_entity =
			    malloc ((unsigned) (strlen (is -> is_entity) + 1)))
		        == NULL)
		    adios (NULLCP, "out of memory");
		(void) strcpy (dz -> dp_entity, is -> is_entity);
		dz -> dp_port = na -> na_port;		
		dz++;
	    }
	}
no_more: ;
    (void) endisoservent ();
}

/*  */

static  envinit () {
    int     i,
            sd;

    nbits = getdtablesize ();

    if (debug == 0 && !(debug = isatty (2))) {
	for (i = 0; i < 5; i++) {
	    switch (fork ()) {
		case NOTOK: 
		    sleep (5);
		    continue;

		case OK: 
		    break;

		default: 
		    _exit (0);
	    }
	    break;
	}

	(void) chdir ("/");

	if ((sd = open ("/dev/null", O_RDWR)) == NOTOK)
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

#ifdef	SETSID
	if (setsid () == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed", "setsid");
#endif
#ifdef	TIOCNOTTY
	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
#else
#ifdef	SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
    }
    else
	ll_dbinit (pgm_log, myname);

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, myname);
    advise (LLOG_NOTICE, NULLCP, "starting");
}

/*    ERRORS */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif
