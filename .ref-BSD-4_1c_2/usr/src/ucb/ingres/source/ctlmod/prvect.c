# include	"ctlmod.h"
# include	<tree.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)prvect.c	7.1	2/5/81)

/*
**  PRVECT -- prints parameter vector
**
**	Prvect merely prints all of the entries in a PARM.  If
**	a tree is included in the PARM, then the tree is printed.
**
**	Parameters:
**		pc -- parameter count
**			if 0 then pv must be PV_EOF terminated.
**		pv -- parameter vector (PARM style).
**			if NULL, the current pv is printed.
**		routine -- header to be printed with pv.
**
**	Returns:
**		nothing
*/

prvect(pc, pv)
int			pc;
register PARM		*pv;
{
	register int	pno;

	if (pv == NULL)
	{
		pc = Ctx.ctx_pc;
		pv = Ctx.ctx_pv;
	}

	for (pno = 0; pv->pv_type != PV_EOF && pno < pc; pv++, pno++)
	{
		printf("   %3d ", pno);
		pr_parm(pv);
	}
}
/*
**  PR_PARM -- print a single parameter
**
**	Parameters:
**		pv -- ptr to the parameter to print.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

pr_parm(pv)
register PARM	*pv;
{
	register int	i;
	register char	*p;

	printf("(len=%3d): ", pv->pv_len);
	switch (pv->pv_type)
	{
	  case PV_INT:
		printf("%d\n", pv->pv_val.pv_int);
		break;

	  case PV_STR:
		printf("\"");
		for (p = pv->pv_val.pv_str; *p != '\0'; p++)
			xputchar(*p);
		printf("\"\n");
		break;

	  case PV_TUPLE:
		p = pv->pv_val.pv_tuple;
		for (i = pv->pv_len; i > 0; i--)
			printf("\%o", *p++);
		printf("\n");
		break;

	  case PV_QTREE:
		treepr(pv->pv_val.pv_qtree);
		break;
	
	  default:
		printf("Unknown type %d\n", pv->pv_type);
		break;
	}
}
