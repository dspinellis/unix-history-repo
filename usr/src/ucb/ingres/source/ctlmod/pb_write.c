# include	"ctlmod.h"
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)pb_write.c	7.1	2/5/81)

/*
**  PB_WRITE -- Write pipe block
**
**	Writes the block specified by ppb to the pipe implied by
**	ppb->pb_proc.  Also handles broadcase messages.
**
**	Parameters:
**		ppb -- a ptr to the pipe block to write.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		12.10 - 12.15
*/

pb_write(ppb)
register pb_t	*ppb;
{
	register int	i;

# ifdef xCTR1
	if (tTf(12, 10))
	{
		lprintf("pb_write: ");
		pb_dump(ppb, TRUE);
	}
# endif

	/* handle broadcast messages */
	if (ppb->pb_proc == PB_WILD)
	{
		/* write on all pipes marked as a broadcast pipe */
		for (i = 0; i < CM_MAXPROC; i++)
		{
			if ((Cm.cm_proc[i].pr_stat & PR_BCAST) != 0)
				pb_wphys(ppb, Cm.cm_proc[i].pr_file);
		}
	}
	else
	{
		/* normal message */
		i = ppb->pb_proc;
# ifdef xCM_DEBUG
		if (i > CM_MAXPROC || i < 0)
			syserr("pb_write: proc %d", i);
# endif
# ifdef xCTR1
		if (tTf(12, 11))
			printf("pb_write: proc %d fd=%d\n", i, Cm.cm_proc[i].pr_file);
# endif
		pb_wphys(ppb, Cm.cm_proc[i].pr_file);
	}

	/* determine the new input */
	if ((ppb->pb_stat & (PB_INFO|PB_EOF)) == PB_EOF)
		Cm.cm_input = Cm.cm_proc[ppb->pb_proc].pr_ninput;

	/* reset some exciting pointers */
	ppb->pb_xptr = ppb->pb_data;
	ppb->pb_nleft = PB_DBSIZE;
	ppb->pb_nused = 0;
}
