# include	<pv.h>
# include	<ingres.h>
# include 	<func.h>
# include	<sccs.h>

SCCSID(@(#)rupdate.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	rupdate();
extern	int	null_fn();

struct fn_def RupdatFn =
{
	"RUPDATE",
	rupdate,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**  RUBOUT SETUP FOR DEFFERED UPDATE PROCESSOR
**
**	These routines setup the special processing for the rubout
**	signal for the deferred update processor.  The update
**	processor is then called.
*/

rupdate(pc, pv)
int	pc;
PARM	pv[];
{
	register int	rtval;

	/* set up special signal processing */
	ruboff("batch update");

	/* call update */
	rtval = update(pc, pv);

	/* clean up signals */
	rubon();

	return (rtval);

}
