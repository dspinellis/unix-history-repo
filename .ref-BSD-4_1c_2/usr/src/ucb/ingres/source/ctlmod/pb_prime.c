# include	"ctlmod.h"
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)pb_prime.c	7.1	2/5/81)

/*
**  PB_PRIME -- prime a pipe for reading or writing
**
**	This clears out any nasty stuff in the pipe block.  If
**	we are reading, it reads the first block so that we can
**	know what sort it is.
**
**	Parameters:
**		ppb -- a pointer to the pipe block.
**		type -- if PB_NOTYPE, we are setting for reading.
**			Otherwise, we are setting to write a
**			message of the indicated type.
**
**	Returns:
**		none
**
**	Side Effects:
**		Changes *ppb.
**
**	Trace Flags:
**		12.0
*/

pb_prime(ppb, type)
register pb_t	*ppb;
int		type;
{
# ifdef xCTR2
	if (tTf(12, 0))
		lprintf("pb_prime: type %d\n", type);
# endif
	if (type == PB_NOTYPE)
	{
		/* read pipe prime -- get the first block */
		pb_read(ppb);
	}
	else
	{
		/* write pipe prime -- set up initial pointers */
		ppb->pb_from = Cm.cm_myproc;
		ppb->pb_type = type;
		ppb->pb_stat = 0;
		ppb->pb_nleft = PB_DBSIZE;
		ppb->pb_nused = 0;
	}

	/* do common initialization */
	ppb->pb_xptr = ppb->pb_data;
}
