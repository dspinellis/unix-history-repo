# include	<useful.h>
# include	<ingres.h>
# include	<aux.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIreadinp.c	7.3	4/26/81)


static char	IInbuf[70];
static char	*IInb_free;

char	*
IIneed(i)
register 	i;
{
	register char	*p;

	p = IInb_free;
	if (IInb_free + i - IInbuf >= sizeof IInbuf)
		IIsyserr("Error param overflow");
	IInb_free += i;
	return (p);
}

IInd_init()
{
	IInb_free = IInbuf;
}

/*
**  IIREADINPUT -- read the input pipe 
**
**	The input pipe is read (using pb_get).  Parameters are
**	collected and set up in the global IIerr_pv.  
**
**	If an error block is read, the error routine processing
**	is invoked.
**
**	Parameters:
**		ppb -- a pointer to the pipe block to read into;
**			also becomes part of the return value.
**
**	Returns:
**
**	Requires:
**		pb_prime, pb_get -- to read from the pipe.
**		IIneed -- to get space to store things
**			from the pipe.
**
**	History:
**		8/21/79 (eric) -- written.
*/

IIreadinput(ppb)
register pb_t	*ppb;
{
	register int	i;
	int		pc;
	char		*pv[20];
	auto int	eno;

	/* if this is a response block, return immediately */
	if (ppb->pb_type == PB_RESP)
	{
		i = IIpb_get(ppb, &IIresp, sizeof IIresp);
		if (i != sizeof IIresp)
			IIsyserr("readinput: IIresp sz %d", i);
		IItupcnt = IIresp.resp_tups;
	}

	/*
	**  Parameter Loop.
	**	Wander through and start reading parameters.
	*/

	IInd_init();
	for (pc = 0 ; pc < PV_MAXPC; pc++)
	{
		if (IIread_arg(ppb, &pv[pc]) == PV_EOF)
			break;
	}

	/* out of loop, check for vector overflow */
	if (pc >= sizeof pv / sizeof pv[0])
		IIsyserr("readinput: overflow");

	/* check for error blocks */
	if (ppb->pb_type == PB_ERR)
	{
		IIatoi(pv[0], &eno);
		IIerror(eno, pc - 1, &pv[1]);
	}
}
/*
**  IIREAD_ARG -- Read a single argument from pipe
**
**	An argument can be as simple as an integer, or as complex
**	as a query tree.
**
**	Parameters:
**		ppb -- the pipe block to read from.
**		pparm -- the parameter descripter to put the
**			argument in.
**
**	Returns:
**		PV_EOF -- if last arg read
**
**	Side Effects:
**		May allocate space from Qbuf for trees, etc.
**
**	Requires:
**		pb_get
**		syserr
**		need
**		readqtree
**
**	Called By:
**		readinput
**
**	Trace Flags:
**		10.6 - 10.7
*/

IIread_arg(ppb, parm)
register pb_t	*ppb;
char		**parm;
{
	char		buf[20];
	auto char	ptype;
	auto short	plen;
	register int	i;
	register char	*p;
	char		*ib;
	int		j;
	char		*IIneed();
	char		*IIitos();

	/* get the parameter type */
	i = IIpb_get(ppb, &ptype, 1);
	if (i == 0)
	{
		return (PV_EOF);
	}
	i = IIpb_get(ppb, &plen, 2);
	if (i < 2)
		IIsyserr("readarg: pb_get %d", i);

	/* figure out the type */
	switch (ptype)
	{
	  case PV_INT:
		IIpb_get(ppb, &j, plen);
		ib = IIitos(j);
		p = IIneed(j = IIlength(ib) + 1);
		IIbmove(ib, p, j);
		break;

	  case PV_STR:
		p = IIneed(plen + 1);
		IIpb_get(ppb, p, plen);
		p[plen] = 0;
		break;

	  default:
		IIsyserr("readinput: type %d len %d", ptype, plen);
	}
	*parm = p;
	return (ptype);
}
