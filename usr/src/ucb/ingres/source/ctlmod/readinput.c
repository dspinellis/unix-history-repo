# include	"ctlmod.h"
# include	"pipes.h"
# include	<resp.h>
# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<sccs.h>

SCCSID(@(#)readinput.c	7.2	2/16/81)

/*
**  READINPUT -- read the input pipe and determine next function
**
**	The input pipe is read (using pb_get).  Parameters are
**	collected and set up in the global Ctx.ctx_pv.  *ppb
**	is set to the new function, block type, etc.
**
**	If an error block is read, the error routine processing
**	is invoked.
**
**	Parameters:
**		ppb -- a pointer to the pipe block to read into;
**			also becomes part of the return value.
**
**	Returns:
**		nothing directly.
**
**	Side Effects:
**		Sets Ctx.ctx_pv, Ctx.ctx_pc to the new parmv &
**			parmc read from the pipe.
**		Sets the *ppb struct to indicate state, etc.
**
**	Trace Flags:
**		10.0 - 10.5
*/

readinput(ppb)
register pb_t	*ppb;
{
	register int	i;

	/*
	**  Top Loop.
	**	Executed once for each complete block read.  Normally
	**	only executed once, but can be more if an error
	**	block is read.
	**
	**	We mark Qbuf first, so we can free any parameters
	**	when they are no longer needed (such as when they
	**	are passed to another process).
	*/

	Ctx.ctx_pmark = markbuf(Qbuf);
# ifdef xCTR1
	if (tTf(10, 0))
		lprintf("readinput: mark %d, errfn %x, ppb %x\n", Ctx.ctx_pmark, Ctx.ctx_errfn, ppb);
# endif

	for (;;)
	{
		/* prime the input (reads first block) */
		pb_prime(ppb, PB_NOTYPE);
# ifdef xCTR2
		if (tTf(10, 1))
			lprintf("readinput: type %d\n", ppb->pb_type);
# endif

		/* if this is a response block, return immediately */
		if (ppb->pb_type == PB_RESP)
		{
			i = pb_get(ppb, (char *) &Resp, sizeof Resp);
			if (i != sizeof Resp)
				syserr("readinput: Resp sz %d", i);
/*
			read_arg(ppb, &Resp.resp_rval);
*/
			break;
		}

		/*
		**  Parameter Loop.
		**	Wander through and start reading parameters.
		*/

		for (Ctx.ctx_pc = 0; Ctx.ctx_pc < PV_MAXPC; Ctx.ctx_pc++)
		{
			if (read_arg(ppb, &Ctx.ctx_pv[Ctx.ctx_pc]) == PV_EOF)
				break;
		}

		/* out of loop, check for vector overflow */
		if (Ctx.ctx_pc >= PV_MAXPC)
			syserr("readinput: overflow");

		/* check for error blocks */
		if (ppb->pb_type == PB_ERR)
		{
			proc_err(ppb, Ctx.ctx_pc, Ctx.ctx_pv);
			syserr("readinput: proc_err");
		}


		/* non-error block */
# ifdef xCM_DEBUG
		if (ppb->pb_type != PB_REG)
			syserr("readinput: pb_type %d", ppb->pb_type);
# endif
		Ctx.ctx_resp = ppb->pb_resp;
		break;
	}
# ifdef xCTR1
	if (tTf(10, 4))
	{
		lprintf("readinput: ");
		pb_dump(ppb, FALSE);
	}
# endif
}
/*
**  READ_ARG -- Read a single argument from pipe
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
**		none.
**
**	Side Effects:
**		May allocate space from Qbuf for trees, etc.
**
**	Called By:
**		readinput
**
**	Trace Flags:
**		10.6 - 10.7
*/

read_arg(ppb, pparm)
register pb_t	*ppb;
register PARM	*pparm;
{
	auto char	ptype;
	auto short	plen;
	register int	i;
	register char	*p;
	QTREE		*q;
	extern char	*need();
	extern QTREE	*readqry();

	/* get the parameter type */
	i = pb_get(ppb, &ptype, 1);
	if (i == 0)
	{
		pparm->pv_type = PV_EOF;
		pparm->pv_val.pv_str = NULL;
		return (PV_EOF);
	}
	i = pb_get(ppb, (char *) &plen, 2);
	if (i < 2)
		syserr("readarg: pb_get %d", i);

	/* figure out the type */
	switch (ptype)
	{
	  case PV_INT:
# ifdef xCM_DEBUG
		if (plen != sizeof pparm->pv_val.pv_int)
			syserr("readinput: PV_INT %d", plen);
# endif
		pb_get(ppb, (char *) &pparm->pv_val.pv_int, plen);
		break;

	  case PV_STR:
	  case PV_TUPLE:
		p = need(Qbuf, plen);
		pb_get(ppb, p, plen);
		pparm->pv_val.pv_str = p;
		break;

	  case PV_QTREE:
		q = readqry(pb_get, (int) ppb, TRUE);
		pparm->pv_val.pv_qtree = q;
		break;

	  case PV_EOF:
		/* this case is allowed for the mon-par interface */
		break;

	  default:
		syserr("readinput: type %d len %d", ptype, plen);
	}

	/* save the type & length */
	pparm->pv_type = ptype;
	pparm->pv_len = plen;

	return (ptype);
}
