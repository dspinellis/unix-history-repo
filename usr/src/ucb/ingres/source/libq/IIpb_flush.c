# include	<pipes.h>
# include	<sccs.h>

SCCSID(@(#)IIpb_flush.c	7.1	2/5/81)


/*
**  IIPB_FLUSH -- flush a pipe buffer
**
**	This routine insures that all the data in a pipe buffer
**	is flushed out to the pipe.
**
**	We also handle input switching in this routine.  If the
**	message we are writing is not merely informational (such
**	as an error message, or some sort of meta message), we
**	change the input to be whatever pipe the named process
**	will write back on.
**
**	Parameters:
**		ppb -- a ptr to the pipe buffer to flush.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

IIpb_flush(ppb)
register pb_t	*ppb;
{
	/* mark this as an EOF block and flush the buffer */
	ppb->pb_stat |= PB_EOF;
	IIpb_write(ppb);
}
