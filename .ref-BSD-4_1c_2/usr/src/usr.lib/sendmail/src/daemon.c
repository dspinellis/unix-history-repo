# include <errno.h>
# include "sendmail.h"

#ifndef DAEMON
SCCSID(@(#)daemon.c	3.51		2/26/83	(w/o daemon mode));
#else

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <wait.h>

SCCSID(@(#)daemon.c	3.51		2/26/83	(with daemon mode));

/*
**  DAEMON.C -- routines to use when running as a daemon.
**
**	This entire file is highly dependent on the 4.2 BSD
**	interprocess communication primitives.  No attempt has
**	been made to make this file portable to Version 7,
**	Version 6, MPX files, etc.  If you should try such a
**	thing yourself, I recommend chucking the entire file
**	and starting from scratch.  Basic semantics are:
**
**	getrequests()
**		Opens a port and initiates a connection.
**		Returns in a child.  Must set InChannel and
**		OutChannel appropriately.
**	clrdaemon()
**		Close any open files associated with getting
**		the connection; this is used when running the queue,
**		etc., to avoid having extra file descriptors during
**		the queue run and to avoid confusing the network
**		code (if it cares).
**	makeconnection(host, port, outfile, infile)
**		Make a connection to the named host on the given
**		port.  Set *outfile and *infile to the files
**		appropriate for communication.  Returns zero on
**		success, else an exit status describing the
**		error.
**
**	The semantics of both of these should be clean.
*/
/*
**  GETREQUESTS -- open mail IPC port and get requests.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Waits until some interesting activity occurs.  When
**		it does, a child is created to process it, and the
**		parent waits for completion.  Return from this
**		routine is always in the child.  The file pointers
**		"InChannel" and "OutChannel" should be set to point
**		to the communication channel.
*/

struct sockaddr_in	SendmailAddress;/* internet address of sendmail */
int	DaemonSocket = -1;		/* fd describing socket */
int	MaxConnections = 4;		/* maximum simultaneous sendmails */

getrequests()
{
	int t;
	union wait status;
	int numconnections = 0;
	register struct servent *sp;

	/*
	**  Set up the address for the mailer.
	*/

	sp = getservbyname("smtp", "tcp");
	if (sp == NULL)
	{
		syserr("server \"smtp\" unknown");
		goto severe;
	}
	SendmailAddress.sin_family = AF_INET;
	SendmailAddress.sin_addr.s_addr = INADDR_ANY;
	SendmailAddress.sin_port = sp->s_port;

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(15, 1))
		printf("getrequests: port 0x%x\n", SendmailAddress.sin_port);
# endif DEBUG

	/* get a socket for the SMTP connection */
	DaemonSocket = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (DaemonSocket < 0)
	{
		/* probably another daemon already */
		syserr("getrequests: can't create socket");
	  severe:
# ifdef LOG
		if (LogLevel > 0)
			syslog(LOG_SALERT, "cannot get connection");
# endif LOG
		finis();
	}

#ifdef DEBUG
	/* turn on network debugging? */
	if (tTd(15, 15))
		(void) setsockopt(DaemonSocket, SOL_SOCKET, SO_DEBUG, 0, 0);
#endif DEBUG

	if (bind(DaemonSocket, &SendmailAddress, sizeof SendmailAddress, 0) < 0)
	{
		syserr("getrequests: cannot bind");
		(void) close(DaemonSocket);
		goto severe;
	}
	listen(DaemonSocket, 10);

# ifdef DEBUG
	if (tTd(15, 1))
		printf("getrequests: %d\n", DaemonSocket);
# endif DEBUG

	for (;;)
	{
		auto int lotherend;
		struct sockaddr_in otherend;

		/* wait for a connection */
		register int pid;

		do
		{
			errno = 0;
			lotherend = sizeof otherend;
			t = accept(DaemonSocket, &otherend, &lotherend, 0);
		} while (t < 0 && errno == EINTR);
		if (t < 0)
		{
			syserr("getrequests: accept");
			sleep(5);
			continue;
		}

		/*
		**  Create a subprocess to process the mail.
		*/

# ifdef DEBUG
		if (tTd(15, 2))
			printf("getrequests: forking (fd = %d)\n", t);
# endif DEBUG

		pid = fork();
		if (pid < 0)
		{
			syserr("daemon: cannot fork");
			sleep(10);
			(void) close(t);
			continue;
		}

		if (pid == 0)
		{
			extern struct hostent *gethostbyaddr();
			register struct hostent *hp;
			extern char *RealHostName;	/* srvrsmtp.c */
			char buf[MAXNAME];

			/*
			**  CHILD -- return to caller.
			**	Collect verified idea of sending host.
			**	Verify calling user id if possible here.
			*/

			/* determine host name */
			hp = gethostbyaddr(&otherend.sin_addr, sizeof otherend.sin_addr, AF_INET);
			if (hp != NULL)
				(void) sprintf(buf, "%s.ARPA", hp->h_name);
			else
				/* this should produce a dotted quad */
				(void) sprintf(buf, "%lx", otherend.sin_addr.s_addr);
			RealHostName = newstr(buf);

			(void) close(DaemonSocket);
			InChannel = fdopen(t, "r");
			OutChannel = fdopen(t, "w");
# ifdef DEBUG
			if (tTd(15, 2))
				printf("getreq: returning\n");
# endif DEBUG
# ifdef LOG
			if (LogLevel > 11)
				syslog(LOG_DEBUG, "connected, pid=%d", getpid());
# endif LOG
			return;
		}

		/*
		**  PARENT -- wait for child to terminate.
		**	Perhaps we should allow concurrent processing?
		*/

# ifdef DEBUG
		if (tTd(15, 2))
		{
			sleep(2);
			printf("getreq: parent waiting\n");
		}
# endif DEBUG

		/* close the port so that others will hang (for a while) */
		(void) close(t);

		/* pick up old zombies; implement load limiting */
		numconnections++;
		while (wait3(&status, numconnections < MaxConnections ? WNOHANG : 0, 0) > 0)
			numconnections--;
	}
	/*NOTREACHED*/
}
/*
**  CLRDAEMON -- reset the daemon connection
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		releases any resources used by the passive daemon.
*/

clrdaemon()
{
	if (DaemonSocket >= 0)
		(void) close(DaemonSocket);
	DaemonSocket = -1;
}
/*
**  MAKECONNECTION -- make a connection to an SMTP socket on another machine.
**
**	Parameters:
**		host -- the name of the host.
**		port -- the port number to connect to.
**		outfile -- a pointer to a place to put the outfile
**			descriptor.
**		infile -- ditto for infile.
**
**	Returns:
**		An exit code telling whether the connection could be
**			made and if not why not.
**
**	Side Effects:
**		none.
*/

makeconnection(host, port, outfile, infile)
	char *host;
	u_short port;
	FILE **outfile;
	FILE **infile;
{
	register int s;

	/*
	**  Set up the address for the mailer.
	**	Accept "[a.b.c.d]" syntax for host name.
	*/

	if (host[0] == '[')
	{
		long hid;
		register char *p = index(host, ']');

		if (p != NULL)
		{
			*p = '\0';
			hid = inet_addr(&host[1]);
			*p = ']';
		}
		if (p == NULL || hid == -1)
		{
			usrerr("Invalid numeric domain spec \"%s\"", host);
			return (EX_NOHOST);
		}
		SendmailAddress.sin_addr.s_addr = hid;
	}
	else
	{
		register struct hostent *hp = gethostbyname(host);

		if (hp == NULL)
			return (EX_NOHOST);
		bmove(hp->h_addr, (char *) &SendmailAddress.sin_addr, hp->h_length);
	}

	/*
	**  Determine the port number.
	*/

	if (port != 0)
		SendmailAddress.sin_port = htons(port);
	else
	{
		register struct servent *sp = getservbyname("smtp", "tcp");

		if (sp == NULL)
		{
			syserr("makeconnection: server \"smtp\" unknown");
			return (EX_OSFILE);
		}
		SendmailAddress.sin_port = sp->s_port;
	}

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection (%s)\n", host);
# endif DEBUG

	s = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (s < 0)
	{
		syserr("makeconnection: no socket");
		goto failure;
	}

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection: %d\n", s);

	/* turn on network debugging? */
	if (tTd(16, 14))
		(void) setsockopt(s, SOL_SOCKET, SO_DEBUG, 0, 0);
# endif DEBUG
	(void) fflush(CurEnv->e_xfp);			/* for debugging */
	SendmailAddress.sin_family = AF_INET;
	if (connect(s, &SendmailAddress, sizeof SendmailAddress, 0) < 0)
	{
		/* failure, decide if temporary or not */
	failure:
		switch (errno)
		{
		  case EISCONN:
		  case ETIMEDOUT:
		  case EINPROGRESS:
		  case EALREADY:
		  case EADDRINUSE:
		  case EHOSTDOWN:
		  case ENETDOWN:
		  case ENETRESET:
		  case ENOBUFS:
		  case ECONNREFUSED:
		  case EHOSTUNREACH:
		  case ENETUNREACH:
			/* there are others, I'm sure..... */
			return (EX_TEMPFAIL);

		  case EPERM:
			/* why is this happening? */
			syserr("makeconnection: funny failure, addr=%lx, port=%x",
				SendmailAddress.sin_addr.s_addr, SendmailAddress.sin_port);
			/* explicit fall-through */

		  default:
			return (EX_UNAVAILABLE);
		}
	}

	/* connection ok, put it into canonical form */
	*outfile = fdopen(s, "w");
	*infile = fdopen(s, "r");

	return (EX_OK);
}
/*
**  MYHOSTNAME -- return the name of this host.
**
**	Parameters:
**		hostbuf -- a place to return the name of this host.
**
**	Returns:
**		A list of aliases for this host.
**
**	Side Effects:
**		none.
*/

char **
myhostname(hostbuf)
	char hostbuf[];
{
	extern struct hostent *gethostbyname();
	struct hostent *hp;
	auto int i = 30;
	register char *p;

	gethostname(hostbuf, &i);
	hp = gethostbyname(hostbuf);
	for (p = hostbuf; *p != '\0'; p++)
		if (islower(*p))
			*p -= 'a' - 'A';
	if (hp != NULL)
		return (hp->h_aliases);
	else
		return (NULL);
}

# else DAEMON

/*
**  MYHOSTNAME -- stub version for case of no daemon code.
**
**	Can't convert to upper case here because might be a UUCP name.
*/

char **
myhostname(hostbuf)
	char hostbuf[];
{
	register FILE *f;

	hostbuf[0] = '\0';
	f = fopen("/usr/include/whoami", "r");
	if (f != NULL)
	{
		(void) fgets(hostbuf, sizeof hostbuf, f);
		fixcrlf(hostbuf, TRUE);
		(void) fclose(f);
	}
	return (NULL);
}

#endif DAEMON
