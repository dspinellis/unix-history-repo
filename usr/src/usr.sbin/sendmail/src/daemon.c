# include "sendmail.h"

static char	SccsId[] =	"@(#)daemon.c	3.4	%G%";

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
	syserr("Daemon mode not yet implemented");
	exit(EX_USAGE);

	for (;;)
	{
		register int pid;
		auto int st;

		/*
		**  Wait for a connection.
		*/

		/* MailPort = getconnection(); */

		pid = fork();
		if (pid < 0)
		{
			syserr("daemon: cannot fork");
			sleep(10);
			continue;
		}

		if (pid == 0)
		{
			/*
			**  CHILD -- return to caller.
			**	Verify calling user id if possible here.
			*/

			initsys();
			return;
		}

		/*
		**  PARENT -- wait for child to terminate.
		**	Perhaps we should allow concurrent processing?
		*/

		(void) wait(&st);
	}
}
