# include	"ctlmod.h"
# include	<sccs.h>

SCCSID(@(#)cm_reset.c	7.1	2/5/81)

/*
**  CM_RESET -- reset control module
**
**	Called on a RESET block, this routine should reset the
**	state to something well known.  Useful for debugging.
**
**	Parameters:
**		none
**
**	Returns:
**		never -- non-locally to top of CM loop.
**
**	Side Effects:
**		'Syncs' is cleared.
**		'Qbuf' is reset.
**		'Cm.cm_input' is set to the default input.
**		All extra files are closed.
**
**	Trace Flags:
**		none
*/

cm_reset()
{
	register int	i;
	extern jmp_buf	CmReset;
	extern long	CmOfiles;

	/* clear all Syncs */
	for (i = 0; i < CM_MAXPROC; i++)
		Syncs[i] = 0;

	/* close and report all extraneous files */
	closeall(TRUE, CmOfiles);

	/* return to top of loop */
	longjmp(CmReset, 3);
}
