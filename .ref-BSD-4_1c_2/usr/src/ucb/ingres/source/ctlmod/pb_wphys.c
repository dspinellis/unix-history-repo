# include	<useful.h>
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)pb_wphys.c	7.1	2/5/81)

/*
**  PB_WPHYS -- physical write on pipe
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
**	Called By:
**		pb_write
**
**	Trace Flags:
**		none
*/

pb_wphys(ppb, fd)
register pb_t	*ppb;
register int	fd;
{
	register int	i;

	i = write(fd, (char *) ppb, PB_IOSIZE);
	if (i != PB_IOSIZE)
	{
		pb_dump(ppb, TRUE);
		syserr("pb_wphys: write error: fd=%d, i=%d", fd, i);
	}
}
