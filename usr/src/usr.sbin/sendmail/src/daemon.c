# include "sendmail.h"

static char	SccsId[] =	"@(#)daemon.c	3.2	%G%";

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
	getrecipients();
	exit(EX_USAGE);
	/* initsys(); */
}
/*
**  GETRECIPIENTS -- do a sendto to all recipients.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The set of recipients for this request are
**		collected and designated as recipients.
*/

getrecipients()
{
	return;
}
