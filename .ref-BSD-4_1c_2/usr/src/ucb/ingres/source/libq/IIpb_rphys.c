# include	<pipes.h>
# include	<sccs.h>

SCCSID(@(#)IIpb_rphys.c	7.1	2/5/81)


/*
**  IIPB_RPHYS -- physical read on pipe
**
**	Parameters:
**		ppb -- a pointer to the data area.
**		fd -- the file descriptor.
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

IIpb_rphys(ppb, fd)
register pb_t	*ppb;
register int	fd;
{
	return (read(fd, ppb, PB_IOSIZE));
}
