# include	"ctlmod.h"
# include	<signal.h>
# include	<sccs.h>

SCCSID(@(#)cm_cleanup.c	7.1	2/5/81)

/*
**  CM_CLEANUP -- cleanup after interrupt or error.
**
**	This routine does things like call the interrupt cleanup
**	function, reset the input, etc.
**
**	Parameters:
**		typ -- the type of cleanup:
**			1 -- fatal error (from error [error.c]).
**			2 -- keyboard interrupt.
**
**	Returns:
**		never (uses non-local jump to ctlmod/main.c).
**
**	Side Effects:
**		Proc_name & Cm.cm_input are reset.
**
**	Trace Flags:
**		0
*/

cm_cleanup(typ)
int	typ;
{
	register int		i;
	register struct fn_def	*f;
	extern char		*Proc_name;
	extern jmp_buf		CmReset;
	extern			rubcatch();
	register ctx_t		*ctx;

# ifdef xCTR2
	if (tTf(0, 13))
		printf("cm_cleanup: %d\n", typ);
# endif

	/*
	**  Call all interrupt cleanup functions for active
	**	modules.
	*/

	for (i = 0; i < NumFunc; i++)
	{
		f = FuncVect[i];
		if (f->fn_active > 0)
		{
			Ctx.ctx_name = Proc_name = f->fn_name;
			(*f->fn_cleanup)(typ);
		}
	}

	/* clean up memory */
	for (ctx = &Ctx; ctx != NULL; ctx = ctx->ctx_link)
	{
		if (ctx->ctx_qt != NULL)
			free(ctx->ctx_qt);
		if (ctx->ctx_glob != NULL)
		{
			bmove(ctx->ctx_glob, ctx->ctx_fn->fn_gptr, ctx->ctx_fn->fn_gsize);
			free(ctx->ctx_glob);
		}
	}

	/* return to top of loop */
	longjmp(CmReset, typ);
}
