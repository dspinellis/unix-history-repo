#ifndef lint
static char	*sccsid = "@(#)netaux.c	1.8	(Berkeley) 7/17/87";
#endif

/*
 * Routines to deal with network stuff for
 * stand-alone version of server.
 */

#include "common.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <signal.h>

#ifdef ALONE


/*
 * disassociate this process from the invoker's terminal.
 * Close all file descriptors, and then open 0, 1, and 2 to
 * somewhere bogus (i.e., "/", O_RDONLY).  This way we will know
 * that stdin/out/err will at least be claimed.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Disassociates this process from
 *			a terminal; closes file descriptors;
 *			fd 0-2 opened as O_RDONLY to /.
 */

disassoc()
{
	register int	i;

	if (fork())
		exit(0);

	for (i = 0; i < 10; i++)
		(void) close(i);

	i = open("/dev/tty", O_RDWR);
	if (i >= 0) {
		ioctl(i, TIOCNOTTY, 0);
		(void) close(i);
	}

	i = open("/", O_RDONLY);
	if (i >= 0) {
		if (i != 0) {			/* should never happen */
			(void) dup2(i, 0);
			(void) close(i);
		}
		(void) dup2(0, 1);
		(void) dup2(1, 2);
	}
}


/*
 * get_socket -- create a socket bound to the appropriate
 *	port number.
 *
 *	Parameters:	None.
 *
 *	Returns:	Socket bound to correct address.
 *
 *	Side effects:	None.
 *
 *	Errors:		Syslogd, cause aboriton.
 */

get_socket()
{
	int			s;
	struct sockaddr_in	sin;
	struct servent		*sp;

	sp = getservbyname("nntp", "tcp");
	if (sp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "get_socket: tcp/nntp, unknown service.");
#endif
		exit(1);
	}

	bzero((char *) &sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = htonl(INADDR_ANY);
	sin.sin_port = sp->s_port;

	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "get_socket: socket: %m");
#endif
		exit(1);
	}

	if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "get_socket: bind: %m");
#endif
		exit(1);
	}

	return (s);
}

/*
 * make_stdio -- make a given socket be our standard input
 *	and output.
 *
 *	Parameters:	"sockt" is the socket we want to
 *			be file descriptors 0, 1, and 2.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

make_stdio(sockt)
	int	sockt;
{
	if (sockt != 0) {
		(void) dup2(sockt, 0);
		(void) close(sockt);
	}
	(void) dup2(0, 1);
	(void) dup2(1, 2);
}

/*
 * set_timer -- set up the interval timer so that
 *	the active file is read in every so often.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Sets interval timer to READINTVL seconds.
 *			Sets SIGALRM to call read_again.
 */

set_timer()
{
	struct itimerval	new, old;
	extern int		read_again();

	(void) signal(SIGALRM, read_again);

	new.it_value.tv_sec = READINTVL;
	new.it_value.tv_usec = 0;
	new.it_interval.tv_sec = READINTVL;
	new.it_interval.tv_usec = 0;
	old.it_value.tv_sec = 0;
	old.it_value.tv_usec = 0;
	old.it_interval.tv_sec = 0;
	old.it_interval.tv_usec = 0;

	if (setitimer(ITIMER_REAL, &new, &old) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "set_timer: setitimer: %m\n");
#endif
		exit(1);
	}
}


/*
 * read_again -- (maybe) read in the active file again,
 *	if it's changed since the last time we checked.
 *
 *	Parameters:	None (called by interrupt).
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	May change "num_groups" and "group_array".
 */

read_again()
{
	static long	last_mtime;	/* Last time active file was changed */
	struct stat	statbuf;

	if (stat(activefile, &statbuf) < 0)
		return;

	if (statbuf.st_mtime != last_mtime) {
		last_mtime = statbuf.st_mtime;
		num_groups = read_groups();
	}
}


/*
 * reaper -- reap children who are ready to die.
 *	Called by signal.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

reaper()
{
	union wait	status;

	while (wait3(&status, WNOHANG, (struct rusage *)0) > 0)
		;
}

#endif
