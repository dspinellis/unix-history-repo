# include	<stdio.h>
# include	<sgtty.h>
# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)set_so_buf.c	7.1	2/5/81)

/*
**  SET_SO_BUF -- set standard output buffer conditionally
**
**	This routine sets the standard output buffer conditionally,
**	based on whether or not it is a terminal.  If it is, it
**	does not set the output; otherwise, it buffers the output.
**	The buffer is contained internally.
**
**	Parameters:
**		none
**
**	Returns:
**		TRUE -- if buffer was set
**		FALSE -- otherwise
**
**	Side Effects:
**		The standard output is left buffered or unchanged.
*/

set_so_buf()
{
	extern int	errno;
	struct sgttyb	gttybuf;
	static char	buffer[BUFSIZ];

	/* check for standard output is tty */
	if (gtty(fileno(stdout), &gttybuf))
	{
		/* no: reset errno and buffer */
		errno = 0;
		setbuf(stdout, buffer);

		return (TRUE);
	}

	return (FALSE);
}
