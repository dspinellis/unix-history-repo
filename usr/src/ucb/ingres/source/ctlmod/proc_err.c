# include	"ctlmod.h"
# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)proc_err.c	7.1	2/5/81)

/*
**  PROC_ERR -- Process error message
**
**	This routine processes an error.  It searches back through
**	the chain of contexts until it finds one that is willing to
**	process this error, or it finds that it must transfer the
**	error to another process to handle it.
**
**	It also unwinds the tree of
**	activations.  It leaves us in the context that processes
**	the error, or the state that should be reading the
**	input pipe.
**
**	The local error handling function returns zero if the
**	error should be ignored, anything else otherwise.
**
**	Parameters:
**		pc -- error parameter count.
**		pv -- error parameter vector.
**		ppb -- a pointer to a pipe block to use in
**			sending this error.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Unwinds the list of activations.
**		The input pipe can be changed under certain
**			circumstances.
**
**	Trace Flags:
**		6.8 - 6.15
*/


proc_err(ppb, pc, pv)
pb_t	*ppb;
int	pc;
PARM	pv[];
{
	register struct fn_def	*f;
	extern char		*Proc_name;
	register int		i;
	register ctx_t		*ctx;
	extern pb_t		*MonPpb;

# ifdef xCTR2
	if (tTf(6, 8))
		lprintf("proc_err: new = %d\n", Ctx.ctx_new);
# endif
	pb_prime(ppb, PB_ERR);

	/*
	**  Scan back on the list of context dependencies.
	**	If we come to someone who can process this message,
	**	we go ahead and do it.  We also take this
	**	opportunity to unwind the context list & call the
	**	cleanup functions.
	*/

	for (ctx = &Ctx; ctx != NULL; ctx = ctx->ctx_link)
	{
		Proc_name = ctx->ctx_name;
		f = ctx->ctx_fn;
# ifdef xCTR2
		if (tTf(6, 9))
			lprintf("proc_err: unwinding %s: errfn=%x, ppb=%x, link=%x, resp=%d, fn=%x\n",
			    Proc_name, ctx->ctx_errfn, ctx->ctx_ppb,
			    ctx->ctx_link, ctx->ctx_resp, f);
# endif

		/*  Do the actual error processing. */
		ppb->pb_proc = ctx->ctx_resp;
		if (ctx->ctx_errfn != NULL)
			i = (*ctx->ctx_errfn)(pc, pv);
		else
			i = -1;

# ifdef xCTR2
		if (tTf(6, 11))
			lprintf("proc_err: errcode %d\n", i);
# endif
		if (i == 0)
			break;
		else if (i > 0)
		{
			/* turn into nonfatal error */
			ppb->pb_stat |= PB_INFO;
			ppb->pb_proc = PB_FRONT;
		}
		else
		{
			/* call the cleanup function */
			if (f != NULL && f->fn_active > 0)
				(*f->fn_cleanup)(1);
		}

		/* arrange to leave if parent not in this process */
		if (ppb->pb_proc != Cm.cm_myproc)
		{
			send_off(ppb, pc, pv);
			pb_flush(ppb);

			/* throw away dead contexts and exit */
			break;
		}
	}
	if (ctx == NULL)
		syserr("proc_err: no parent");

# ifdef xCTR3
	if (tTf(6, 12))
	{
		lprintf("proc_err: cleanup: ctx=%x, ->_link=%x, MonPpb = ", ctx, ctx->ctx_link);
		pb_dump(MonPpb, TRUE);
	}
# endif
	/* pop contexts down to ctx and exit */
	ctx = ctx->ctx_link;
	while (Ctx.ctx_link != ctx)
	{
		if (Ctx.ctx_link == NULL)
			syserr("proc_err: underflow");
		Ctx.ctx_new = TRUE;
		resetp();
	}

	/*
	**  Flush input pipe.
	**	THIS CODE IS ONLY NEEDED TO MAKE READMON WORK, AND
	**	SHOULD BE REMOVED WHEN READMON GOES AWAY!!
	*/

	if (ctx == NULL)
	{
		Cm.cm_input = Cm.cm_rinput;
		while (!bitset(PB_EOF, MonPpb->pb_stat))
			pb_read(MonPpb);
		MonPpb->pb_st = PB_UNKNOWN;
	}

	longjmp(Ctx.ctx_jbuf, 1);
}
