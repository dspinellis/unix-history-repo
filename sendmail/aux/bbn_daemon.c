# include <errno.h>
# include "sendmail.h"

#ifndef DAEMON
SCCSID(@(#)bbn_daemon.c	3.36+		12/13/82	(w/o daemon mode));
#else
SCCSID(@(#)bbn_daemon.c	3.36+		12/13/82	(w/ daemon mode));

/*
**  DAEMON.C -- routines to use when running as a daemon.
**
**	BB&N Version.  Hack attack!!  This version is not supported,
**	and probably has some bugs......
**
**	Basic semantics are:
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

#include "netlib.h"
#include "con.h"
#include <wait.h>

#define SMTPSOCK	25
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
	int t;
	int iretval;
	union wait status;
	struct con openparams;

	/*
	**  Set up the address for the mailer.
	*/

	bzero (&openparams, sizeof openparams); /* clear out the structure */


	openparams.c_mode = CONTCP;
	openparams.c_lport = SMTPSOCK;
	openparams.c_rbufs = 2;		/* 2k receive buffer */

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(15, 1))
		printf("getrequests\n");
# endif DEBUG

	for (;;)
	{
		/* wait for a connection */
		register int pid;

		do
		{
			errno = 0;
			t = open("/dev/net/net", &openparams);
		} while (t < 0 && errno == EINTR);
		if (t < 0)
		{
			syserr("getrequests: open");
			sleep(500);
			continue;
		}


		/* set 'eol' mode in connection */

		ioctl (t, NETSETE, 0);

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
			/*
			**  CHILD -- return to caller.
			**	Verify calling user id if possible here.
			*/

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
		if ((iretval = close(t)) < 0)
			syserr("getreq:  cannot close port 25:  %d", iretval) ;


		/* pick up old zombies; implement load limiting */
		while (wait3(&status, WNOHANG, 0) > 0)
			continue;
	}
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
	struct con openparams;

	bzero (&openparams, sizeof openparams); /* clear structure's memory */
	
	openparams.c_mode = CONACT | CONTCP;
	openparams.c_timeo = 60;

	/*
	**  Set up the address for the mailer.
	**	Accept "[a.b.c.d]" syntax for host name.
	*/

	if (host[0] == '[')
	{
		long hid = 0;
		int i, j;
		register char *p = host;

		for (i = 3; i >= 0 && *p != ']' && *p != '\0'; i--)
		{
			j = 0;
			while (isdigit(*++p))
				j = j * 10 + (*p - '0');
			if (*p != (i == 0 ? ']' : '.') || j > 255 || j < 0)
				break;
			hid |= j << ((3 - i) * 8);
		}
		if (i >= 0 || *p != ']' || *++p != '\0')
		{
			usrerr("Invalid numeric domain spec \"%s\"", host);
			return (EX_NOHOST);
		}
		openparams.c_fcon._na_l = hid;
	}
	else
	{

		openparams.c_fcon = gethost(host);
		if (isbadhost(openparams.c_fcon))
			return (EX_NOHOST);
	}

	/*
	**  Determine the port number.
	*/

	if (port == 0)
		openparams.c_fport = SMTPSOCK;
	else
		openparams.c_fport = port;

	/*
	**  Try to actually open the connection.
	*/

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection(%s=%X)\n", host, openparams.c_fcon._na_l);
# endif DEBUG

	s = open("/dev/net/net", &openparams);

# ifdef DEBUG
	if (tTd(16, 1))
		printf("makeconnection: %d\n", s);
# endif DEBUG

	if (s < 0)
		return (EX_TEMPFAIL);

	/* connection ok, put it into canonical form */
	*outfile = fdopen(s, "w");
	*infile = fdopen(s, "r");

	return (0);
}

# endif DAEMON
/*
**  MYHOSTNAME -- return the name of this host.
**
**	This is a hideous hack....  It should be fixed to really work
**	under the BB&N code.  However, since Brendan's configuration
**	tables don't use this feature it won't matter.
**
**	Parameters:
**		hostbuf -- a place to return the name of this host.
**		size -- the size of hostbuf.
**
**	Returns:
**		A list of aliases for this host.
**		NULL if it cannot be found.
**
**	Side Effects:
**		none.
*/

char **
myhostname(hostbuf, size)
	char hostbuf[];
	int size;
{
	register FILE *f;

	hostbuf[0] = '\0';
	f = fopen("/usr/include/whoami", "r");
	if (f != NULL)
	{
		(void) fgets(hostbuf, size, f);
		fixcrlf(hostbuf, TRUE);
		(void) fclose(f);
	}
	return (NULL);
}
