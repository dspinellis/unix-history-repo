/* tp0bridge.c - TCP/X.25 TP0 bridge */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/tp0bridge/RCS/tp0bridge.c,v 7.2 91/02/22 09:34:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/tp0bridge/RCS/tp0bridge.c,v 7.2 91/02/22 09:34:27 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University in the UK
 *
 *
 * $Log:	tp0bridge.c,v $
 * Revision 7.2  91/02/22  09:34:27  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:42:52  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:11:02  mrose
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


#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "tpkt.h"
#include <sys/ioctl.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include "internet.h"
#include "x25.h"
#include "logger.h"
#include "tailor.h"

/*  */

static int debug = 0;
static int options = 0;
static int  nbits = FD_SETSIZE;

static LLog _pgm_log = {
    "tp0bridge.log", NULLCP, NULLCP,
    LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE, LLOG_FATAL, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;

static char *myname = "tp0bridge";
static char myhost[64];

static char *myprotocol = "tcp";
static char *myservice = "x25bridge";

static int	tcp_fd;
static int      nfds;
static fd_set	ifds;
static struct sockaddr_in   main_in_socket;
static struct sockaddr_in  *mainisock = &main_in_socket;
static struct sockaddr_in   emptyaddr;

static	fd_set	sentinel;
static	int	sent2list[FD_SETSIZE];
static	int	list2sent[FD_SETSIZE];
static struct sockaddr_in   callbacks[FD_SETSIZE];
#ifdef	CAMTEC
static struct NSAPaddr		    listens[FD_SETSIZE];
#define	FORK_LISTENER		/* new process per listener */
#endif


void	abortfd (), shuffle ();
void	adios (), advise ();
#ifdef	BSD42
int	chldser ();
#else
int	alrmser ();
#endif


extern int  errno;

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int     fd;
    fd_set  mask;

    arginit (argv);
    envinit ();

    nfds = 0;
    FD_ZERO (&ifds);
    FD_ZERO (&sentinel);

    if ((tcp_fd = start_tcp_server (mainisock, SOMAXCONN, options & SO_DEBUG
		    ? SO_DEBUG : 0, 0)) == NOTOK)
	adios ("failed", "start_tcp_server");
    FD_SET (tcp_fd, &ifds);
    if (tcp_fd >= nfds)
	nfds = tcp_fd + 1;

#ifdef	BSD42
    (void) signal (SIGCHLD, chldser);
#endif

    for (;;) {
	mask = ifds;

#ifdef CAMTEC
	/* due to problems in select when camtec is enabled, a small
	   timeout is given. This should not affect anything I hope.
	  */
	(void) xselect (nfds, &mask, NULLFD, NULLFD, 1);
#else
	(void) xselect (nfds, &mask, NULLFD, NULLFD, NOTOK);
#endif

	for (fd = 0; fd < nbits; fd++)
	    if (FD_ISSET (fd, &mask)) {
		if (fd == tcp_fd)
		    do_new_fd ();
		else if (FD_ISSET (fd, &sentinel))
		    abortfd (fd);
		else
		    do_old_fd (fd);
	    }

#ifndef	BSD42
	while (mywait3 (NULLIP) != NOTOK)
	    continue;
#endif
    }
}

/*  */

static	do_new_fd ()
{
    int     fd;
    char    c;
    struct sockaddr_in zosock;
    struct sockaddr_in *osock = & zosock;
	
    if ((fd = join_tcp_client (tcp_fd, osock)) == NOTOK) {
	if (errno != EINTR)
	    advise (LLOG_EXCEPTIONS, "failed", "join_tcp_client");
	return;
    }

    if (read_tcp_socket (fd, &c, 1) != 1) {
	advise (LLOG_EXCEPTIONS, "failed", "initial read_tcp_socket");
	goto out;
    }

    switch (c) {
	case 1:
#ifdef DEBUG
	    advise (LLOG_DEBUG, NULLCP, "outgoing connection");
#endif
	    switch (fork ()) {
		case OK: 
		    do_outgoing (fd);
		    _exit (1);		/* NOTREACHED */

		case NOTOK: 
		    advise (LLOG_EXCEPTIONS, "TCP socket",
			    "no forks, so rejecting connection on");
		default: 
		    (void) close_tcp_socket (fd);
#ifdef	EXOS
		    FD_CLR (tcp_fd, &ifds);
		    if ((tcp_fd = start_tcp_server (isock, SOMAXCONN,
				    options & SO_DEBUG ? SO_DEBUG : 0, 0))
			    == NOTOK)
			adios ("failed", "start_tcp_server");
		    FD_SET (tcp_fd, &ifds);
		    if (tcp_fd >= nfds)
			nfds = tcp_fd + 1;
#endif
		    break;
	    }
	    break;

	case 2:
#ifdef DEBUG
	    advise (LLOG_DEBUG, NULLCP, "setup listen address");
#endif
	    do_listen (fd);
	    break;

	default:
	    advise (LLOG_NOTICE, NULLCP, "unknown dialogue mode 0x%x", c);
    out:    ;
	    (void) close_tcp_socket (fd);
	    break;

    }
}

/*  */

static	do_outgoing (fd)
int	fd;
{
    int		sd;
    struct NSAPaddr	znsap;
    register struct NSAPaddr *nsap = &znsap;

#ifdef	SOCKETS
    (void) close_tcp_socket (tcp_fd);
#endif
    for (sd = 0; sd < nbits; sd++)
	if (sd != fd && FD_ISSET (sd, &ifds))
	    (void) close (sd);
#ifdef	BSD42
    (void) signal (SIGCHLD, SIG_DFL);
#endif

    (void) ll_close (pgm_log);

    if (bridge_read_nsap_addr (fd, nsap, read_tcp_socket) == NOTOK)
	adios ("failed", "read of NSAP");

    switch (nsap -> na_stack) {
        case NA_BRG:
	    nsap -> na_stack = NA_X25;	/* we use real X.25 here */
	    break;
	case NA_X25:
	    break;
	case NA_NSAP:
	case NA_TCP:
	default:
	    adios (NULLCP, "Addressing style not supported (type %d)",
		   nsap -> na_stack);
	    break;
    }
	
#ifdef DEBUG
    advise(LLOG_DEBUG, NULLCP, "x25 call on to %*s\n",
	   nsap->na_dtelen, nsap->na_dte);
#endif
    if ((sd = start_x25_client (nsap)) == NOTOK)
	adios ("failed", "start_x25_client");

    if (join_x25_server (sd, nsap) == NOTOK)
	adios ("failed", "join_x25_server");

    transfer (sd, fd);
}

/*  */

static	do_listen (fd)
int	fd;
{
    struct sockaddr_in  in_socket;
    register struct sockaddr_in *isock = &in_socket;
    struct NSAPaddr	znsap;
    register struct NSAPaddr *nsap = &znsap;
    int		newfd;

    if (bridge_read_nsap_addr (fd, nsap, read_tcp_socket) == NOTOK) {
	advise (LLOG_EXCEPTIONS, "failed", "read of NSAP");
	goto out;
    }
    switch (nsap -> na_stack) {
        case NA_BRG:
	    nsap -> na_stack = NA_X25;	/* we use real X.25 here */
	    break;
	case NA_X25:
	    break;
	case NA_NSAP:
	case NA_TCP:
	default:
	    adios (NULLCP, "Addressing style not supported (type %d)",
		   nsap -> na_stack);
	    break;
    }

#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "type=%d Listening on '%s' len=%d",
	    nsap -> na_stack, nsap -> na_dte, nsap -> na_dtelen);
    advise (LLOG_DEBUG, NULLCP, "pid='%s'(%d) fac='%s'(%d) cudf='%s'(%d)",
	    nsap -> na_pid, nsap -> na_pidlen,
	    nsap -> na_fac, nsap -> na_faclen,
	    nsap -> na_pid, nsap -> na_pidlen,
	    nsap -> na_cudf, nsap -> na_cudflen);
#endif
    if (readx (fd, (char *) isock, sizeof *isock, read_tcp_socket)
	!= sizeof *isock) {
	advise (LLOG_EXCEPTIONS, "failed",
		"read_tcp_socket of sockaddr_in");
	goto out;
    }
    isock -> sin_family = ntohs (isock -> sin_family);

#ifdef FORK_LISTENER
    switch (fork ()) {
	case OK:
	{
	    int i;

	    for (i = 0; i < nbits; i++)
		if (i != fd && FD_ISSET (i, &ifds))
		    (void) close (i);
	    break;
	}
	case NOTOK: 
	    advise (LLOG_EXCEPTIONS, "X25 socket",
		    "no forks, so rejecting listen on");
	default: 
	    (void) close_tcp_socket (fd);
	    return;
    }
	/* more from the 'stamp out boring names' committee */
#define iso_defining_new_protocols 1
    while (iso_defining_new_protocols)
    {
#endif
    if ((newfd = start_x25_server (nsap, SOMAXCONN, options & SO_DEBUG
		    ? SO_DEBUG : 0, 0)) == NOTOK) {
	advise (LLOG_EXCEPTIONS, "failed", "start_x25_server");
#ifdef FORK_LISTENER
	_exit (0);
#endif
	return;
    }
#ifdef	CAMTEC
    listens[newfd] = *nsap;	/* struct copy */
#endif
    callbacks[newfd] = *isock;	/* struct copy */

    /* set up the admin stuff */
    FD_SET (newfd, &ifds);	/* listen for new connections */
    FD_SET (fd, &ifds);		/* listen for problems */
    FD_SET (fd, &sentinel);
    sent2list[fd] = newfd;
    list2sent[newfd] = fd;
    if (newfd >= nfds)
	nfds = newfd + 1;
#ifdef FORK_LISTENER
    do_old_fd (newfd);
    (void) close (newfd);
    } /* loop again */
#endif
    return;

out: ;
    (void) close_tcp_socket (fd);
}


/*  */

static	do_old_fd (fd)
int	fd;
{
    int	    sd;
    register struct sockaddr_in *isock = &callbacks[fd];
    struct NSAPaddr	znsap;
    register struct NSAPaddr *nsap = &znsap;

#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "callback");
#endif
    if (isock == NULL || isock -> sin_family != AF_INET) {
	advise (LLOG_EXCEPTIONS, NULLCP, "Callback has bogus address");
	return;
    }

    if ((sd = join_x25_client (fd, nsap)) == NOTOK) {
	if (errno != EINTR) {
	    advise (LLOG_EXCEPTIONS, "failed", "join_x25_client");
	    abortfd (list2sent[fd]);
	}
	return;
    }
#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "type=%d Accepted '%s' len=%d",
	    nsap -> na_stack, nsap -> na_dte, nsap -> na_dtelen);
    advise (LLOG_DEBUG, NULLCP, "pid='%s'(%d) fac='%s'(%d) cudf='%s'(%d)",
	    nsap -> na_pid, nsap -> na_pidlen,
	    nsap -> na_fac, nsap -> na_faclen,
	    nsap -> na_pid, nsap -> na_pidlen,
	    nsap -> na_cudf, nsap -> na_cudflen);
#endif
    nsap -> na_stack = NA_BRG;

#ifdef	CAMTEC
	/* get back addressing info for this fd into nsap
	 */
	nsap = &listens[fd];
#endif

    switch (fork ()) {
	case OK: 
	    break;

	case NOTOK: 
	    advise (LLOG_EXCEPTIONS, "X.25 socket",
		    "no forks, so rejecting connection on");
	default: 
	    (void) close (sd);
	    return;
    }

#ifndef	CAMTEC
    (void) close (fd);
#endif
    for (fd = 0; fd < nbits; fd++)
	if (fd != sd && FD_ISSET (fd, &ifds))
	    (void) close (fd);
#ifdef	BSD42
    (void) signal (SIGCHLD, SIG_DFL);
#endif

    (void) ll_close (pgm_log);

#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "connecting to %s", inet_ntoa (isock -> sin_addr));
    advise (LLOG_DEBUG, NULLCP, "port=%d", (int) ntohs (isock -> sin_port));
#endif

    if ((fd = start_tcp_client ((struct sockaddr_in *) NULL, 0)) == NOTOK)
	adios ("failed", "start_tcp_client");
    if (join_tcp_server (fd, isock) == NOTOK)
	adios ("failed", "join_tcp_server");

    if (bridge_write_nsap_addr (fd, nsap, write_tcp_socket) == NOTOK)
	adios ("failed", "write of NSAP");

    transfer (sd, fd);
    _exit (1);		/* NOTREACHED */
}

/*  */

static	transfer (sd, fd)
int	sd,
	fd;
{
    int	    mfds;
    fd_set  rfds,
            mask;
    struct tsapblk *tcpb,
                   *x25b;

    FD_ZERO (&rfds);
    mfds = (sd > fd ? sd : fd) + 1;
    FD_SET (sd, &rfds);
    FD_SET (fd, &rfds);

    if ((tcpb = newtblk ()) == NULL)
	adios (NULLCP, "out of memory");
    tcpb -> tb_fd = fd;
    (void) TTService (tcpb);

    if ((x25b = newtblk ()) == NULL)
	adios (NULLCP, "out of memory");
    x25b -> tb_fd = sd;
    (void) XTService (x25b);

#ifndef	CAMTEC
    for (;;) {
	mask = rfds;
	if (xselect (mfds, &mask, NULLFD, NULLFD, NOTOK) == NOTOK)
	    adios ("failed", "xselect");
	if (FD_ISSET (sd, &mask))
	    shuffle (x25b, tcpb);
	if (FD_ISSET (fd, &mask))
	    shuffle (tcpb, x25b);
    }
#else
    for (;;) {
	FD_ZERO (&mask);
	mfds = fd + 1;
	FD_SET (fd, &mask);
	if (xselect (mfds, &mask, NULLFD, NULLFD, 1) == NOTOK)
	    adios ("failed", "xselect tcp");
	if (FD_ISSET (fd, &mask)) {
	    advise (LLOG_DEBUG, NULLCP, "tcp -> x25");
	    shuffle (tcpb, x25b);
	}

	FD_ZERO (&mask);
	mfds = sd + 1;
	FD_SET (sd, &mask);
	if (xselect (mfds, &mask, NULLFD, NULLFD, 1) == NOTOK)
	    adios ("failed", "xselect x25");
	if (FD_ISSET (sd, &mask)) {
	    advise (LLOG_DEBUG, NULLCP, "x25 -> tcp");
	    shuffle (x25b, tcpb);
	}
    }
#endif
}

/*  */

static	void	shuffle (from, to)
register struct tsapblk *from,
			*to;
{
    register struct udvec  *uv;
    register struct tsapkt *t;

    if ((t = fd2tpkt (from -> tb_fd, from -> tb_initfnx, from -> tb_readfnx))
	    == NULL || t -> t_errno != OK)
	adios (NULLCP, "fd2tpkt failed (%d)", t ? t -> t_errno : NOTOK);
#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "read: code=0x%x user len=%d",
	    TPDU_CODE (t), t -> t_qbuf ? t -> t_qbuf -> qb_len : 0);
#endif

    uv = t -> t_udvec;
    if (t -> t_qbuf) {
	uv -> uv_base = t -> t_qbuf -> qb_data;
	uv -> uv_len = t -> t_qbuf -> qb_len;
	uv++;
    }
    uv -> uv_base = NULL;

    if (tpkt2fd (to -> tb_fd, t, to -> tb_writefnx) == NOTOK)
	adios (NULLCP, "tpkt2fd failed (%d)", t -> t_errno);

    freetpkt (t);
}

static	void	abortfd (fd)
int	fd;
{
    int	assocfd = sent2list[fd];	/* get fd that is listening on */

#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "Shutdown listen (%d,%d)", fd, assocfd);
#endif
    FD_CLR (fd, &sentinel);
    FD_CLR (fd, &ifds);
    FD_CLR (assocfd, &ifds);
    (void) close_tcp_socket (fd);
    (void) close_x25_socket (assocfd);
    sent2list[fd] = -1;
    list2sent[assocfd] = -1;
    callbacks[assocfd] = emptyaddr;	/* struct copy */
}

/*  */

static	arginit (vec)
char	**vec;
{
    int	    port;
    register char  *ap;
    struct hostent *hp;
    struct servent *sp;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 0);
    ll_hdinit (pgm_log, myname);

    (void) gethostname (myhost, sizeof myhost);
    if (hp = gethostbyname (myhost))
	(void) strcpy (myhost, hp -> h_name);

    if ((sp = getservbyname (myservice, myprotocol)) == NULL)
    	mainisock -> sin_port = x25_bridge_port;
    else
	mainisock -> sin_port = sp -> s_port;	
    mainisock -> sin_family = AF_INET;
    mainisock -> sin_addr.s_addr = INADDR_ANY;
    if ((nbits = getdtablesize ()) > FD_SETSIZE)
	nbits = FD_SETSIZE;

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    continue;

		case 'p': 
		    if ((ap = *++vec) == NULL
			    || *ap == '-'
			    || (port = atoi (ap)) <= 0)
			adios (NULLCP, "usage: %s -p portno", myname);
		    mainisock -> sin_port = htons ((u_short) port);
		    continue;

		default: 
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", myname);
    }
}

/*  */

static  envinit () {
    int     i,
            sd;

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
#endif
    }
    else {
	options |= SO_DEBUG;
	ll_dbinit (pgm_log, myname);
    }

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, myname);
    advise (LLOG_NOTICE, NULLCP, "starting");
#ifdef DEBUG
    advise (LLOG_DEBUG, NULLCP, "options=0x%x port=%d",
	    options, ntohs (mainisock -> sin_port));
#endif
}

/*    Berkeley UNIX: 4.2 */

#ifdef	BSD42

#include <sys/wait.h>

/* ARGSUSED */

static int  chldser (sig, code, sc)
int	sig;
long    code;
struct sigcontext *sc;
{
    union wait status;

    while (wait3 (&status, WNOHANG, (struct rusage *) NULL) > 0)
	continue;
}

#else

/*    AT&T UNIX: 5 */

#include <setjmp.h>

#define	WAITSECS	((unsigned) 2)


static jmp_buf jmpenv;


/* ARGSUSED */

static int  alrmser (sig)
int	sig;
{
     (void) signal (SIGALRM, alrmser);

     longjmp (jmpenv, NOTOK);
}


static int  mywait3 (status)
int    *status;
{
    int     result;

    switch (setjmp (jmpenv)) {
	case OK:
	     (void) signal (SIGALRM, alrmser);
	     (void) alarm (WAITSECS);
	     result = wait (status);
	     (void) alarm (0);
	     break;

	default:
	     result = NOTOK;
	     break;
     }

    return result;
}
#endif

/*  */

static int  readx (fd, buffer, n, readfnx)
int     fd;
char    *buffer;
int     n;
IFP     readfnx;
{
    register int    i,
    cc;
    register char   *bp;

    for (bp = buffer, i = n; i > 0; bp += cc, i -= cc) {
	switch (cc = (*readfnx) (fd, bp, i)) {
	    case NOTOK:
	        return (i = bp - buffer) ? i : NOTOK;

	    case OK:
		break;

	    default:
		continue;
	}
	break;
    }

    return (bp - buffer);
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
