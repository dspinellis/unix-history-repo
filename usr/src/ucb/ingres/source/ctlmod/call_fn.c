# include	"ctlmod.h"
# include	<resp.h>
# include	<sccs.h>

SCCSID(@(#)call_fn.c	7.1	2/5/81)

/*
**  CALL_FN -- call a local function
**
**	This routine, given a pointer to a local function descriptor,
**	calls the local function.
**
**	Parameters:
**		fno -- function definition vector number.
**		pc -- the parameter count
**		pv -- the parameter vector, gets passed to the
**			function.
**
**	Returns:
**		none
**
**	Side Effects:
**		Sets 'Resp' to the response vector for this function.
**		The old 'Resp' is completely obliterated.
**
**	Trace Flags:
**		3
*/

# ifdef xMONITOR
struct monitor	MonBuf[CM_MAXST];
# endif xMONITOR

call_fn(fno, pc, pv)
int	fno;
int	pc;
PARM	**pv;
{
	register struct fn_def	*f;
	register char		*gp;
	register int		i;
# ifdef xMONITOR
	extern struct monitor	CmMonBuf;
	struct monitor		mon;
	struct monitor		*savemon;
	extern char		*cvt_time();
# endif
	extern char		*Proc_name;
	short			*tvect;
	char			*oldname;
	extern short		*tT;
	extern char		*malloc();

	f = FuncVect[fno];
	if (fno > NumFunc || f->fn_fn == NULL || fno < 0)
		syserr("call_fn: undef fn %d", fno);
	Ctx.ctx_fn = f;
# ifdef xCTR1
	if (tTf(3, 0))
		lprintf("call_fn: fn %d (%s)\n", fno, f->fn_name);
# endif

	/*
	**  Save function globals.
	**	If the function we want to call is already active,
	**	and if it has a global data area, allocate space
	**	and save that area.
	*/

	if (f->fn_active > 0 && f->fn_gptr != NULL)
	{
		/* save globals */
		gp = malloc(f->fn_gsize);
		bmove(f->fn_gptr, gp, f->fn_gsize);
		Ctx.ctx_glob = gp;
	}
	else
		Ctx.ctx_glob = gp = NULL;

	/*
	**  Clear the response vector to a known state and call
	**  the function.
	*/

	oldname = Proc_name;
	Ctx.ctx_name = Proc_name = f->fn_name;
	tvect = tT;
	Ctx.ctx_tvect = tT = f->fn_tvect;
	clrmem(&Resp, sizeof Resp);
	Resp.resp_tups = -1;
	markopen(&Ctx.ctx_ofiles);
# ifdef xCTR2
	if (tTf(3, 1))
	{
		lprintf("call_fn: calling %s\n", Proc_name);
		prvect(pc, pv);
	}
# endif
# ifdef xCTR3
	if (tTf(3, 2))
	{
		lprintf("call_fn: Ctx.ctx_ppb ");
		pb_dump(Ctx.ctx_ppb, FALSE);
	}
# endif xCTR3
# ifdef xMONITOR
	savemon = Ctx.ctx_mon;
	Ctx.ctx_mon = &mon;
	clrmem(&mon, sizeof mon);
	markperf(&mon);
# endif xMONITOR

	i = (*f->fn_fn)(pc, pv);

# ifdef xMONITOR
	markperf(&CmMonBuf);
	Ctx.ctx_mon = savemon;
	if (savemon != NULL)
		add_mon(&mon, savemon);
	add_mon(&mon, &MonBuf[Ctx.ctx_ppb->pb_st]);
# endif xMONITOR
# ifdef xCTR1
	if (tTf(3, 3))
		lprintf("call_fn: returns %d\n", i);
# endif
# ifdef xMONITOR
	if (tTf(0, 0))
		printf("CPU time for %s = %s sec\n", Proc_name,
		       cvt_time(mon.mon_utime + mon.mon_stime));
# endif xMONITOR
	Resp.resp_resp = i;
	closeall(TRUE, Ctx.ctx_ofiles);

	/*
	**  Restore old global memory, if there was any.
	*/

	if (gp != NULL)
	{
		bmove(gp, f->fn_gptr, f->fn_gsize);
		Ctx.ctx_glob = NULL;
		free(gp);
	}
	Ctx.ctx_name = Proc_name = oldname;
	Ctx.ctx_tvect = tT = tvect;
}
