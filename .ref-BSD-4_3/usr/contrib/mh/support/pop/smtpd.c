/* smtpd.c - the stub SMTP server for POP client hosts */


/* Author:	Marshall T. Rose	<MRose@NRTC>	(MTR)
		Northrop Research and Technology Center
		One Research park
		Palos Verdes Peninsula, CA  90274
		213/377-4811

   Date:	Wed May 15 00:04:12 1985
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
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>


#define	NOTOK	(-1)
#define	OK	0

/*  */

extern int  errno;
extern char *sys_siglist[];


int  debug = 0;
static int  nbits = ((sizeof (int)) * 8);
static int  options = 0;


char *myname = "smtpd";
char myhost[BUFSIZ];
static char *myprotocol = "tcp";
static char *myservice = "smtp";

static struct sockaddr_in   in_socket;
static struct sockaddr_in  *isock = &in_socket;

static char *smtphost;
static struct sockaddr_in   sm_socket;
static struct sockaddr_in  *msock = &sm_socket;


void	chldser ();
void	adios (), advise ();

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
	adios (NULL, "%s/%s: unknown service", myprotocol, myservice);
    isock -> sin_family = AF_INET;
    isock -> sin_port = sp -> s_port;
    isock -> sin_addr.s_addr = INADDR_ANY;
    arginit (argv);
    envinit ();
    msock -> sin_port = isock -> sin_port;

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
		(void) wait3 (&status, 0, NULL);
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
		advise (NULL, LOG_WARNING, "%s/%s server has terminated -- %s",
			sp -> s_proto, sp -> s_name, reason);
		continue;
	}
	break;
    }

    closelog ();
    openlog (myname, LOG_PID);
    advise (NULL, LOG_INFO, "restart");
#endif	RESTART

/*  */

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK)
	adios ("socket", "unable to create");
    if (options & SO_DEBUG)
	if (setsockopt (sd, SOL_SOCKET, SO_DEBUG, NULL, 0) == NOTOK)
	    advise ("SO_DEBUG", LOG_WARNING, "unable to set socket option");
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, NULL, 0) == NOTOK)
	advise ("SO_KEEPALIVE", LOG_WARNING, "unable to set socket option");
    if (bind (sd, isock, sizeof *isock) == NOTOK)
	adios ("socket", "unable to bind");

    (void) signal (SIGCHLD, chldser);
    (void) listen (sd, SOMAXCONN);
    for (;;) {
	int     i = sizeof *osock;

	if ((fd = accept (sd, osock, &i)) == NOTOK) {
	    if (errno != EINTR)
		advise ("socket", LOG_WARNING,
		    "unable to accept connection on");
	    continue;
	}
	switch (fork ()) {
	    case OK: 
		(void) close (sd);
		(void) signal (SIGCHLD, SIG_DFL);
		server (fd, osock);
		_exit (0);

	    case NOTOK: 
		advise ("socket", LOG_WARNING,
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
    int     sd;
    u_short port;
    char    buffer[BUFSIZ];
    struct hostent *hp;
    struct in_addr *addr;

    closelog ();
    openlog (myname, LOG_PID);
    port = ntohs (sin -> sin_port);
    addr = &sin -> sin_addr;
    advise (NULL, LOG_INFO, "servicing %s/%d",
	    (hp = gethostbyaddr (addr, sizeof *addr, sin -> sin_family))
	    ? hp -> h_name : inet_ntoa (*addr), port);

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK
	    || connect (sd, msock, sizeof *msock) == NOTOK) {
	advise (smtphost, LOG_WARNING,
		sd != NOTOK ? "unable to connect to" : "socket failed for");
	sprintf (buffer, "451 No %s/%s service available, try %s--%s\r\n",
		myprotocol, myservice, smtphost, "you might get lucky");
	write (fd, buffer, strlen (buffer));
	return;
    }

    advise (NULL, LOG_INFO, "connected to %s", smtphost);
    shuffle (fd, sd);
}

/*  */

shuffle (fd, sd)
int	fd,
	sd;
{
    int     cc,
            ibits,
            obits,
            on,
            scc,
            fcc;
    char   *sbp,
            sibuf[BUFSIZ],
           *fbp,
            fibuf[BUFSIZ];

    on = 1;
    ioctl (fd, FIONBIO, &on);
    ioctl (sd, FIONBIO, &on);

    for (fcc = scc = 0;;) {
	ibits = obits = 0;
	if (fcc)
	    obits |= 1 << sd;
	else
	    ibits |= 1 << fd;
	if (scc)
	    obits |= 1 << fd;
	else
	    ibits |= 1 << sd;
	if (fcc < 0 && scc < 0)
	    break;

	select (nbits, &ibits, &obits, NULL, NULL);

	if (ibits == 0 && obits == 0) {
	    sleep (5);
	    continue;
	}

	if (ibits & (1 << fd)) {
	    fcc = read (fd, fibuf, sizeof fibuf);
	    if (fcc < 0 && errno == EWOULDBLOCK)
		fcc = 0;
	    else {
		if (fcc <= 0)
		    break;
		fbp = fibuf;
	    }
	}
	if (ibits & (1 << sd)) {
	    scc = read (sd, sibuf, sizeof sibuf);
	    if (scc < 0 && errno == EWOULDBLOCK)
		scc = 0;
	    else {
		if (scc <= 0)
		    break;
		sbp = sibuf;
	    }
	}

	if ((obits & (1 << fd)) && scc > 0) {
	    cc = write (fd, sbp, scc);
	    if (cc > 0)
		scc -= cc, sbp += cc;
	}
	if ((obits & (1 << sd)) && fcc > 0) {
	    cc = write (sd, fbp, fcc);
	    if (cc > 0)
		fcc -= cc, fbp += cc;
	}
    }

    advise (NULL, LOG_INFO, "terminating: fcc=%d scc=%d errno=%d",
	    fcc, scc, errno);
}

/*  */

/* set options and isock -> sin_port here... */

static	arginit (vec)
char	**vec;
{
    struct hostent *hp;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    (void) gethostname (myhost, sizeof myhost);
    if (hp = gethostbyname (myhost))
	(void) strcpy (myhost, hp -> h_name);
    nbits = getdtablesize ();

    if ((smtphost = *++vec) == NULL)
	adios (NULL, "usage: %s server-host", myname);
    if ((hp = gethostbyname (smtphost)) == NULL)
	adios (NULL," %s: unknown host");
    bzero ((char *) msock, sizeof *msock);
    msock -> sin_family = hp -> h_addrtype;
    bcopy (hp -> h_addr, (char *) &msock -> sin_addr, hp -> h_length);
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
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULL);
	    (void) close (sd);
	}
    }

    for (sd = 3; sd < nbits; sd++)
	(void) close (sd);

    (void) signal (SIGPIPE, SIG_IGN);

    openlog (myname, LOG_PID);
    advise (NULL, LOG_INFO, "starting");
}

/*  */

/* ARGSUSED */

static void chldser (sig, code, sc)
int	sig;
long    code;
struct sigcontext *sc;
{
    union wait status;

    while (wait3 (&status, WNOHANG, NULL) > 0)
	continue;
}

/*  */

/* VARARGS */

void	adios (what, fmt, a, b, c, d)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d;
{
    advise (what, LOG_SALERT, fmt, a, b, c, d);
    _exit (1);
}


/* VARARGS */

void	advise (what, code, fmt, a, b, c, d)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d;
int	code;
{
    char    buffer[BUFSIZ];

    if (what) {
	sprintf (buffer, fmt, a, b, c, d);
	syslog (code, "%s %s: %m", buffer, what);
    }
    else
	syslog (code, fmt, a, b, c, d);

    if (debug) {
	fprintf (stderr, "[%d] ", code);
	fprintf (stderr, fmt, a, b, c, d);
	if (what)
	    (void) fputc (' ', stderr), perror (what);
	else
	    (void) fputc ('\n', stderr);
	(void) fflush (stderr);
    }
}
