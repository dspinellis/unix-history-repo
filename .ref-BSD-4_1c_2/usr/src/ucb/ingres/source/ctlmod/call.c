# include	"ctlmod.h"
# include	"pipes.h"
# include	<resp.h>
# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<sccs.h>

SCCSID(@(#)call.c	7.1	2/5/81)

/*
**  CALL -- Call a state with return.
**
**	The state indicated by the indicated entry point is
**	called.  Control will return to the current state.
**
**	If errflg is non-NULL, it is the address of a function
**	which is willing to handle error messages.  If it is
**	NULL, the parent of this module (and so forth) will
**	handle the error message.
**
**	To call a module, the user should call 'initp' to
**	initialize a context in which the called module will
**	be executed.  Parameters should then be initialized,
**	and then the module should be called.  For instance:
**		initp();
**		init_qt(qmode);   [only if we have a qtree parameter]
**		setp(PV_QTREE, qt, 0);
**		setp(PV_INT, ISAM, 0);
**		setp(PV_TUPLE, tup, tupsiz);
**		call(mdCREATE, catcherr);
**
**		Alternatively, the 'call' can be replaced with:
**		calln(mdCREATE, catcherr);
**		 [save the return value if desired -- not in Qbuf!]
**		resetp();
**
**	Parameters:
**		entpt -- the entry point; indicates the starting
**			state.  If zero, this is an error block.
**		errfn -- the address of an error function;
**			NULL indicates to pass errors to the
**			parent's error handling function.
**
**	Returns:
**		The return value of the final called state.
**
**	Side Effects:
**		Sets 'Resp' to be the response vector for the
**			final called function.
**
**	Trace Flags:
**		5
*/

call(entpt, errfn)
int	entpt;
int	(*errfn)();
{
	calln(entpt, errfn);
	resetp();
	return (Resp.resp_resp);
}
/*
**  CALLN -- just like a call, but keep the context around
**
**	Parameters:
**		(see call)
**
**	Returns:
**		(see call)
**
**	Side Effects:
**		(see call)
*/

calln(entpt, errfn)
int	entpt;
int	(*errfn)();
{
	pb_t			pb;
# ifdef xMONITOR
	extern struct monitor	CmMonBuf;
	extern struct monitor	MonBuf[];
	struct monitor		*savemon;
	struct monitor		mon;
	extern struct monitor	*markperf();
# endif MONITOR

# ifdef xCTR1
	if (tTf(5, 0))
		lprintf("call: %d\n", entpt);
# endif
# ifdef xMONITOR
	savemon = markperf(&CmMonBuf);
# endif xMONITOR

	/*
	**  Select the starting state.
	*/

	if (entpt >= CM_MAXST || entpt < 0)
		syserr("call: entpt %d", entpt);

	/*
	**  Do final setup on context.
	**	Set up the pipe buffer to use in this call.
	**	Flag the end of the parmvect.
	**	Save the error function address.
	**	Save the monitor struct address.  This monitor struct
	**		will save the collective results of this
	**		module & all children.
	*/

	pb_prime(&pb, PB_REG);
	call_setup(&pb, entpt, errfn);
# ifdef xMONITOR
	Ctx.ctx_mon = &mon;
	clrmem(&mon, sizeof mon);
# endif xMONITOR

	/*
	**  Do the sequence.
	*/

# ifdef xCTR1
	if (tTf(5, 2))
		lprintf("call: ENT %d cmark %d pmark %d\n",
		    pb.pb_st, Ctx.ctx_cmark, Ctx.ctx_pmark);
# endif

	Ctx.ctx_new = FALSE;
	do_seq(&pb);
	Ctx.ctx_new = TRUE;

# ifdef xMONITOR
	markperf(savemon);
	Resp.resp_time = mon.mon_utime + mon.mon_stime;
# endif xMONITOR

# ifdef xCTR1
	if (tTf(5, 3))
	{
		lprintf("call: EXIT entpt %d st %d\n", entpt, pb.pb_st);
		printf("\tresp %7d\ttime %7ld\ttups %6ld\n",
			Resp.resp_resp, Resp.resp_time, Resp.resp_tups);
		printf("\tpread %6ld\tpwrit %6ld\n",
			Resp.resp_pread, Resp.resp_pwrit);
	}
# endif

	/*
	**  Return the result of the final function in the chain.
	*/

	return (Resp.resp_resp);
}
/*
**  CALL_SETUP -- Do final setup for call
**
**	This routine just does the final setup before the main
**	part of a call.  It is broken off here because it is
**	also used by 'error'.
**
**	Parameters:
**		ppb -- pointer to a pipe block to use for this
**			call.
**		state -- state we are going to enter.
**		errfn -- a pointer to the error function to use
**			for this call -- NULL if we want to send
**			to our parent.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Does setup on *ppb, Ctx, and Resp.
**
**	Called By:
**		call
**		error
**
**	Trace Flags:
**		none.
*/

call_setup(ppb, state, errfn)
register pb_t	*ppb;
int		state;
int		(*errfn)();
{
# ifdef xCTR1
	if (tTf(5, 5))
	{
		lprintf("call_setup: st %d errfn %x\n", state, errfn);
		prvect(Ctx.ctx_pc, Ctx.ctx_pv);
	}
# endif
	ppb->pb_st = state;
	ppb->pb_resp = Cm.cm_myproc;
	Ctx.ctx_pv[Ctx.ctx_pc].pv_type = PV_EOF;
	Ctx.ctx_pv[Ctx.ctx_pc].pv_val.pv_str = NULL;
	Ctx.ctx_ppb = ppb;
	Ctx.ctx_errfn = errfn;
	Ctx.ctx_init = FALSE;
/*
	Resp.resp_rval.pv_type = PV_EOF;
*/
}
