# include	<pipes.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)IIpb_prime.c	7.2	2/16/81)


/*
**  IIPB_PRIME -- prime a pipe for reading or writing
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

IIpb_prime(ppb, type)
register pb_t	*ppb;
int		type;
{
	if (type == PB_NOTYPE)
	{
		/* read pipe prime -- get the first block */
		IIpb_read(ppb);
	}
	else
	{
		/* write pipe prime -- set up initial pointers */
		ppb->pb_from = ppb->pb_resp = PB_FRONT;
		ppb->pb_type = type;
		ppb->pb_stat = 0;
		ppb->pb_nleft = PB_DBSIZE;
		ppb->pb_nused = 0;
		ppb->pb_st = mdPARSER;
	}

	/* do common initialization */
	ppb->pb_xptr = ppb->pb_data;
}
