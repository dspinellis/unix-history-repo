# include	"ctlmod.h"
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)pb_tput.c	7.1	2/5/81)

/*
**  PB_TPUT -- tagged put
**
**	Puts the symbol out to the pipe with the tag.
**
**	Parameters:
**		tag -- the type of this symbol.
**		dp -- the pointer to the data.
**		len -- the length of the data.
**		ppb -- the pipe buffer to write it on.
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

pb_tput(tag, dp, len, ppb)
int		tag;
char		*dp;
int		len;
register pb_t	*ppb;
{
	auto char	xt;
	auto short	xlen;

	xt = tag;
	pb_put(&xt, 1, ppb);
	xlen = len;
	pb_put((char *) &xlen, 2, ppb);
	pb_put(dp, len, ppb);
}
