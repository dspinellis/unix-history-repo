# include <errno.h>
# include "sendmail.h"
# include <sys/mx.h>

#ifndef DAEMON
SCCSID(@(#)daemon.c	3.22		%G%	(w/o daemon mode));
#else

# include <sys/socket.h>
# include <net/in.h>
# include <wait.h>

SCCSID(@(#)daemon.c	3.22		%G%	(with daemon mode));

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
**	makeconnection(host, port, outfile, infile)
**		Make a connection to the named host on the given
**		port.  Set *outfile and *infile to the files
**		appropriate for communication.  Returns zero on
**		success, else an exit status describing the
**		error.
**
**	The semantics of both of these should be clean.
*/

static FILE	*MailPort;	/* port that mail comes in on */
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
**		routine is always in the child.
*/

# define MAXCONNS	4	/* maximum simultaneous sendmails */

getrequests()
{
	union wait status;
	int numconnections = 0;

	struct wh wbuf;

	wbuf.index = index;
	wbuf.count = 0;
	wbuf.ccount = cnt;
	wbuf.data = buf;
	write(MailPort, &wbuf, sizeof wbuf);
}
/*
**  GETCONNECTION -- make a connection with the outside world
**
**	Parameters:
**		none.
**
**	Returns:
**		The port for mail traffic.
**
**	Side Effects:
**		Waits for a connection.
*/

#define IPPORT_PLAYPORT	3055		/* random number */

struct sockaddr_in SendmailAddress = { AF_INET, IPPORT_SMTP };

getconnection()
{
	register int s;
	struct sockaddr otherend;

	/*
	**  Set up the address for the mailer.
	*/

	SendmailAddress.sin_addr.s_addr = 0;
	SendmailAddress.sin_port = IPPORT_SMTP;
# ifdef DEBUG
	if (tTd(15, 15))
		SendmailAddress.sin_port = IPPORT_PLAYPORT;
# endif DEBUG
	SendmailAddress.sin_port = htons(SendmailAddress.sin_port);

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(15, 1))
		printf("getconnection\n");
# endif DEBUG

	for (;;)
	{
		/* get a socket for the SMTP connection */
		s = socket(SOCK_STREAM, 0, &SendmailAddress, SO_ACCEPTCONN);
		if (s < 0)
		{
			/* probably another daemon already */
			syserr("getconnection: can't create socket");
			break;
		}

# ifdef DEBUG
		if (tTd(15, 1))
			printf("getconnection: %d\n", s);
# endif DEBUG

		/* wait for a connection */
		if (accept(s, &otherend) >= 0)
			break;

		/* probably innocuous -- retry */
		if (errno == ETIMEDOUT)
			continue;
		syserr("getconnection: accept");
		(void) close(s);
		sleep(20);
	}

	return (s);
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
	*/

	if ((SendmailAddress.sin_addr.s_addr = rhost(&host)) == -1)
		return (EX_NOHOST);
	if (port == 0)
		port = IPPORT_SMTP;
	SendmailAddress.sin_port = htons(port);

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection (%s)\n", host);
# endif DEBUG

	s = socket(SOCK_STREAM, 0, (struct sockaddr_in *) 0, 0);
	if (s < 0)
	{
		syserr("makeconnection: no socket");
		goto failure;
	}

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection: %d\n", s);
# endif DEBUG
	(void) fflush(Xscript);				/* for debugging */
	if (connect(s, &SendmailAddress) < 0)
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
		  case ENETDOWN:
		  case ENETRESET:
		  case ENOBUFS:
		  case ECONNREFUSED:
			/* there are others, I'm sure..... */
			return (EX_TEMPFAIL);

		  default:
			return (EX_UNAVAILABLE);
		}
	}

	/* connection ok, put it into canonical form */
	*outfile = fdopen(s, "w");
	*infile = fdopen(s, "r");

	return (0);
}

#endif DAEMON
