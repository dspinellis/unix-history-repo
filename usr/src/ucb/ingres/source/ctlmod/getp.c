# include	"ctlmod.h"
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)getp.c	7.1	2/5/81)

/*
**  GETP -- returns the current PV
**
**	Parameters:
**		none
**
**	Returns:
**		a pointer to the pv. (the PV[PC].pv_type == PV_EOF)
**
**	Side Effects:
**		sets PV[PC].pv_type = PV_EOF.
*/

PARM *
getp()
{
	Ctx.ctx_pv[Ctx.ctx_pc].pv_type = PV_EOF;
	return (Ctx.ctx_pv);
}
