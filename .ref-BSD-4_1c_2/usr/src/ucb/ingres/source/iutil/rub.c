# include	<useful.h>
# include	<signal.h>
# include	<sccs.h>

SCCSID(@(#)rub.c	7.1	2/5/81)

/*
**  RUB -- handle interrupt signals
**
**	This routine arranges to handle the interrupt signal
**	on behalf of the CM.  It sends SYNC blocks to everyone
**	who ought to get them, and arranges to ignore all
**	blocks except SYNC (and RESET) blocks from everyone
**	who will be sending one.  It also calls all of the
**	interrupt functions for all active modules.  It does
**	this after writing all of the sync blocks, so an
**	interrupt function can safely issue a query if it
**	wants.  It then does a non-local goto to the top of
**	the main loop.
**
**	Parameters:
**		none
**
**	Returns:
**		non-local
**
**	Side Effects:
**		Proc_name gets set to the CM name.
**		Interrupt functions for all active modules
**			are invoked.
**		The 'Syncs' vector is updated appropriately.
**		SYNC blocks are sent to all adjacent processes.
**
**	Called By:
**		System, via 'signal' (section 2).
**
**	Trace Flags:
**		none
*/

int	RubLevel;	/* depth of ruboff calls, -1 if off */
char	*RubWhy;	/* why are interrupts disabled */
int	RubGot;		/* set if we have an unprocessed interrupt */

rubcatch()
{
	/* ignore interrupts while we are processing */
	signal(SIGINT, SIG_IGN);

	/* find out if we are allowing interrupts */
	if (RubLevel < 0)
		syserr("bad SIGINT");
	else if (RubLevel > 0)
	{
		/* save interrupt for later processing */
		RubGot++;
		if (RubWhy != NULL)
			printf("Rubout locked out (%s in progress)\n", RubWhy);
	}
	else
	{
		/* do other processing (either from ctlmod or standalones) */
		rubproc();
		syserr("rubcatch: rubproc");
	}
}
/*
**  TURN RUBOUTS OFF
**
**	Further rubouts will be caught by rubsave.
**	The parameter should be the name of the processor which
**	insists that rubouts do not occur.
*/

ruboff(why)
char	*why;
{
	/* check to see if this should be ignored */
	if (RubLevel < 0 || RubLevel++ > 0)
		return;

	/* set up to ignore interrupts */
	RubGot = 0;
	RubWhy = why;
}
/*
**  TURN RUBOUTS BACK ON
**
**	Rubout processing is restored to the norm (calling rubcatch).
**	If any rubouts have occured while disabled, they are processed
**	now.
*/

rubon()
{
	/* check to see if we should leave as-is */
	if (RubLevel < 0 || --RubLevel > 0)
		return;

	/* process any old interrupts */
	if (RubGot > 0)
		rubcatch();

	/* reset state */
	RubWhy = NULL;
}
