# include "sendmail.h"
# include <sys/mx.h>

#ifndef DAEMON
SCCSID(@(#)daemon.c	3.7		%G%	(w/o daemon mode));
#else

# include <sys/socket.h>
# include <wellknown.h>
# include <net/in.h>

SCCSID(@(#)daemon.c	3.7		%G%	(with daemon mode));

/*
**  DAEMON.C -- routines to use when running as a daemon.
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

getrequests()
{
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

struct sockaddr_in SendmailAddress = { AF_INET, IPPORT_SMTP };

getconnection()
{
	register int s;
	char *host = "localhost";
	struct sockaddr otherend;

	/*
	**  Set up the address for the mailer.
	*/

	SendmailAddress.sin_addr.s_addr = rhost(&host);

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (Debug)
		printf("getconnection (%s)\n", host);
# endif DEBUG

	s = socket(SOCK_STREAM, 0, &SendmailAddress, SO_ACCEPTCONN);

# ifdef DEBUG
	if (Debug)
		printf("getconnection: %d\n", s);
# endif DEBUG
	accept(s, &otherend);

	return (s);
}

#endif DAEMON
