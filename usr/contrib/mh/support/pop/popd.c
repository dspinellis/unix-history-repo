/* popd.c - the POP server */

/* Author:	Marshall T. Rose	<MRose@UDel>	(MTR)
		Department of Computer Science and Information Sciences
		University of Delaware
		Newark, DE  19716
		302/451-1951

   Date:	Sun Oct 28 16:23:26 1984
 */

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <strings.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>


#define	NOTOK	(-1)
#define	OK	0

#define	NULLCP	((char *) 0)
#define NULLRP	((struct rusage *) 0)

#define	FAST			/* fast start-up of BBoards */

/*  */

extern int  errno;
extern int  sys_nerr;
extern char *sys_errlist[];
extern char *sys_siglist[];


int  debug = 0;
static int  nbits = ((sizeof (int)) * 8);
static int  options = 0;


char *myname = "popd";
char myhost[BUFSIZ];
static char *myprotocol = "tcp";
static char *myservice = "pop";

static struct sockaddr_in   in_socket;
static struct sockaddr_in  *isock = &in_socket;


int	chldser ();
void	padios (), padvise ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int     fd,
            sd;
    struct servent *sp;
    struct sockaddr_in  out_socket,
                       *osock = &out_socket;

    if ((sp = getservbyname (myservice, myprotocol)) == NULL)
	padios (NULLCP, "%s/%s: unknown service", myprotocol, myservice);
    isock -> sin_family = AF_INET;
    isock -> sin_port = sp -> s_port;
    isock -> sin_addr.s_addr = INADDR_ANY;
    arginit (argv);
    envinit ();

#ifdef	RESTART
    for (;;) {
	char    reason[BUFSIZ];
	union wait status;

	switch (fork ()) {
	    case NOTOK: 
		sleep (5);
		continue;

	    case OK: 
		break;

	    default: 
		sleep (60);
		(void) wait3 (&status, 0, NULLRP);
		if (WIFEXITED (status))
		    (void) sprintf (reason, "exit=0%o", status.w_retcode);
		else
		    if (WIFSIGNALED (status))
			(void) sprintf (reason, "signal=%s%s",
				status.w_termsig < NSIG
				? sys_siglist[status.w_termsig] : "unknown",
				status.w_coredump ? " (core dumped)" : NULL);
		    else
			(void) strcpy (reason, "stopped(!!)");
		padvise (NULLCP, LOG_WARNING, "%s/%s server has terminated -- %s",
			sp -> s_proto, sp -> s_name, reason);
		continue;
	}
	break;
    }

    closelog ();
    openlog (myname, LOG_PID);
    padvise (NULLCP, LOG_INFO, "restart");
#endif	RESTART

/*  */

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK)
	padios ("socket", "unable to create");
    if (options & SO_DEBUG)
	if (setsockopt (sd, SOL_SOCKET, SO_DEBUG, NULL, 0) == NOTOK)
	    padvise ("SO_DEBUG", LOG_WARNING, "unable to set socket option");
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, NULL, 0) == NOTOK)
	padvise ("SO_KEEPALIVE", LOG_WARNING, "unable to set socket option");
    if (bind (sd, (struct sockaddr *) isock, sizeof *isock) == NOTOK)
	padios ("socket", "unable to bind");

    (void) signal (SIGCHLD, chldser);
    (void) listen (sd, SOMAXCONN);
#ifdef	FAST
    popinit ();
#endif	FAST
    for (;;) {
	int     i = sizeof *osock;

	if ((fd = accept (sd, (struct sockaddr *) osock, &i)) == NOTOK) {
	    if (errno != EINTR)
		padvise ("socket", LOG_WARNING,
		    "unable to accept connection on");
	    continue;
	}
#ifdef	FAST
	popassert ();
#endif	FAST
	switch (fork ()) {
	    case OK: 
		(void) close (sd);
		(void) signal (SIGCHLD, SIG_DFL);
		server (fd, osock);
		_exit (0);

	    case NOTOK: 
		padvise ("socket", LOG_WARNING,
		    "no forks, so rejecting connection on");
	    default: 
		(void) close (fd);
	}
    }
}

/*  */

static	server (fd, sin)
int	fd;
struct sockaddr_in *sin;
{
    u_short port;
    struct hostent *hp;
    struct in_addr *addr;

    closelog ();
    openlog (myname, LOG_PID);
    port = ntohs (sin -> sin_port);
    addr = &sin -> sin_addr;
    hp = gethostbyaddr (addr, sizeof *addr, sin -> sin_family);
    padvise (NULLCP, LOG_INFO, "servicing %s/%d",
	    hp ? hp -> h_name : inet_ntoa (*addr), port);

    (void) dup2 (fd, 0);
    (void) close (fd);
    (void) dup2 (0, 1);

    pop (0, 1, sin -> sin_family == AF_INET && port < IPPORT_RESERVED && hp,
	    hp ? hp -> h_name : NULLCP);
}
	
/*  */

static	arginit (vec)
char	**vec;
{
    register char  *ap;
    struct hostent *hp;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    (void) gethostname (myhost, sizeof myhost);
    if (hp = gethostbyname (myhost))
	(void) strcpy (myhost, hp -> h_name);
    nbits = getdtablesize ();

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd': 
		    options |= SO_DEBUG;
		    continue;

		case 'p': 
		    if ((ap = *++vec) == NULL
			    || *ap == '-'
			    || (isock -> sin_port = atoi (ap)) <= 0)
			padios (NULLCP, "usage: %s -p portno", myname);
		    isock -> sin_port = htons (isock -> sin_port);
		    continue;

		default: 
		    padios (NULLCP, "-%s: unknown switch", ap);
	    }

	padios (NULLCP, "usage: %s [switches]", myname);
    }
}

/*  */

static  envinit () {
    int     i,
            sd;

    if (!(debug = isatty (2))) {
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
	    padios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
    }

    for (sd = 3; sd < nbits; sd++)
	(void) close (sd);

    (void) signal (SIGPIPE, SIG_IGN);

    openlog (myname, LOG_PID);
    padvise (NULLCP, LOG_INFO, "starting");
    if (debug)
	padvise (NULLCP, LOG_DEBUG, "options=0x%x port=%d",
		options, ntohs (isock -> sin_port));
}

/*  */

/* ARGSUSED */

static int chldser (sig, code, sc)
int	sig;
long    code;
struct sigcontext *sc;
{
    union wait status;

    while (wait3 (&status, WNOHANG, NULLRP) > 0)
	continue;
}

/*  */

/* VARARGS2 */

void	padios (what, fmt, a, b, c, d, e, f, g, h, i, j)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f,
       *g,
       *h,
       *i,
       *j;
{
    padvise (what, LOG_SALERT, fmt, a, b, c, d, e, f, g, h, i, j);
    _exit (1);
}

/*  */

/* VARARGS3 */

void	padvise (what, code, fmt, a, b, c, d, e, f, g, h, i, j)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f,
       *g,
       *h,
       *i,
       *j;
int	code;
{
    int     eindex = errno;
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, fmt, a, b, c, d, e, f, g, h, i, j);
    if (what)
	if (eindex > 0 && eindex < sys_nerr)
	    syslog (code, "%s %s: %s", buffer, what, sys_errlist[eindex]);
	else
	    syslog (code, "%s %s: Error %d", buffer, what, eindex);
    else
	syslog (code, "%s", buffer);

    if (debug) {
	fprintf (stderr, "[%d] %s", code, buffer);
	if (what)
	    (void) fputc (' ', stderr), perror (what);
	else
	    (void) fputc ('\n', stderr);
	(void) fflush (stderr);
    }

    errno = eindex;
}
