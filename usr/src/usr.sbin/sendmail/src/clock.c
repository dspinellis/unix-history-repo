# include "sendmail.h"

SCCSID(@(#)clock.c	3.1		%G%);

/*
**  TICK -- take a clock tick
**
**	Someday this will have to do more complex event scheduling.
**
**	Parameters:
**		none.
**
**	Returns:
**		non-local through TickFrame.
**
**	Side Effects:
**		none.
*/

tick()
{
# ifdef DEBUG
	if (Debug > 0)
		printf("tick\n");
# endif DEBUG
	longjmp(TickFrame, 1);
}
