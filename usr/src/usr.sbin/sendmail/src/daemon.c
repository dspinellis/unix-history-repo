# include "sendmail.h"

#ifndef DAEMON
SCCSID(@(#)daemon.c	3.6		%G%	(w/o daemon mode));
#else

# include <sys/socket.h>
# include <wellknown.h>
# include <net/in.h>

SCCSID(@(#)daemon.c	3.6		%G%	(with daemon mode));

/*
**  DAEMON.C -- routines to use when running as a daemon.
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
**		routine is always in the child.
*/

getrequests()
{
	for (;;)
	{
		register int pid;
		auto int st;
		register int port;

		/*
		**  Wait for a connection.
		*/

		while ((port = getconnection()) < 0)
		{
			syserr("getrequests: getconnection failed");
			sleep(30);
		}

		/*
		**  Create a subprocess to process the mail.
		*/

# ifdef DEBUG
		if (Debug > 1)
			printf("getrequests: forking (port = %d)\n", port);
# endif DEBUG

		pid = fork();
		if (pid < 0)
		{
			syserr("daemon: cannot fork");
			sleep(10);
			close(port);
			continue;
		}

		if (pid == 0)
		{
			/*
			**  CHILD -- return to caller.
			**	Verify calling user id if possible here.
			*/

			InChannel = fdopen(port, "r");
			OutChannel = fdopen(port, "w");
			initsys();
# ifdef DEBUG
			if (Debug > 1)
				printf("getreq: returning\n");
# endif DEBUG
			return;
		}

		/*
		**  PARENT -- wait for child to terminate.
		**	Perhaps we should allow concurrent processing?
		*/

# ifdef DEBUG
		if (Debug > 1)
		{
			sleep(2);
			printf("getreq: parent waiting\n");
		}
# endif DEBUG

		(void) wait(&st);
		close(port);
	}
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

struct sockaddr_in SendmailAddress = { AF_INET, IPPORT_SENDMAIL };

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
