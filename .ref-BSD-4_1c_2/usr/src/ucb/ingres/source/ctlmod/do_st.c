# include	"ctlmod.h"
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)do_st.c	7.1	2/5/81)

/*
**  DO_ST -- do a state.
**
**	This routine does a state as indicated by ppb, handing it
**	pc & pv as parameters.  The state must be known already.
**
**	If the state is local, it calls 'call_fn' to actually
**	do the setup and call the function.
**	If the state is remote, it uses send_off to send ppb
**	to the process that can do it.
**
**	It returns the next state to execute, possibly PB_UNKNOWN.
**
**	Parameters:
**		ppb -- a pipe block which identifies the state
**			to call, etc.
**		pc -- the pc to hand to the function.
**		pv -- the pv to had to the function.
**
**	Returns:
**		none (except through ppb)
**
**	Side Effects:
**		*ppb gets set to identify the next state.
**		Resp is set if local function.
**
**	Called By:
**		do_seq
**
**	Trace Flags:
**		2.8 - 2.15
*/

do_st(ppb, pc, pv)
register pb_t	*ppb;
int		pc;
PARM		*pv;
{
	register state_t	*s;
	register int		i;
	int			rtval;

	i = ppb->pb_st;
	s = &Cm.cm_state[i];
	if (i < 0 || i > CM_MAXST || s->st_type == ST_UNDEF)
		syserr("do_st: undef state %d", i);
# ifdef xCTR1
	if (tTf(2, 8))
		lprintf("do_st: state %d type %d mark %d\n",
		    i, s->st_type, markbuf(Qbuf));
# endif

	switch (s->st_type)
	{
	  case ST_REMOT:
		/*
		**  Remote: determine the correct process and send
		**  it on its way.
		*/

		ppb->pb_proc = s->st_v.st_rem.st_proc;
		send_off(ppb, pc, pv);
		pb_flush(ppb);
		rtval = PB_UNKNOWN;
		break;

	  case ST_LOCAL:
		/*
		**  Local: execute the function using 'call_fn'
		**  and compute next state.
		**  If call originated in this process, set the EOF bit
		**  so we don't try to read this pipe later.
		*/

		if (bitset(PB_FRFR, ppb->pb_stat) && !bitset(ST_EXTERN, s->st_stat))
			syserr("do_st: restricted state %d", i);
		if (ppb->pb_from == Cm.cm_myproc)
			setbit(PB_EOF, ppb->pb_stat);
		call_fn(s->st_v.st_loc.st_funcno, pc, pv);
		rtval = s->st_v.st_loc.st_next;
		break;

	  default:
		syserr("do_st: type %d", s->st_type);
	}

# ifdef xCTR1
	if (tTf(2, 10))
		lprintf("do_st: ret %d mark %d\n", rtval, markbuf(Qbuf));
# endif
	return (rtval);
}
