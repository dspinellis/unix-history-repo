# include	<ingres.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)pass_err.c	7.1	2/5/81)

/*
**  PASS_ERR -- the error passing routine for the parser
**
**	Trace Flags:
**		pass_err ~~ 65
*/

pass_err(pc, pv)
int	pc;
PARM	pv[];
{
	extern int	Ingerr;
	extern int	Err_fnd;
	register int	num;

	num = pv[0].pv_val.pv_int;
# ifdef	xPTR1
	tTfp(65, 0, "pass_err %d\n", num);
# endif

	if (Ingerr)
		Ingerr = num;
	else
		Ingerr = 1;

	Err_fnd += 1;

	return (-1);		/* means pass error message up to front end */
}
