# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	<aux.h>
# include	"../decomp/globs.h"
# include	"../ctlmod/pipes.h"
# include	<sccs.h>

SCCSID(@(#)startovqp.c	7.1	2/5/81)


/*
**	startovqp is called at the beginning of
**	the execution of ovqp.
*/


startovqp()
{
	extern	flptexcep();

	if (Equel)
		startequel();

	De.ov_tupsfound = 0;	/* counts the number of tuples which sat the qual */
	De.ov_retrieve = De.ov_bopen = FALSE;
	/* catch floating point signals */
	signal(8, (int) flptexcep);
}

/*
**	Give a user error for a floating point exceptions
*/
flptexcep()
{
	ov_err(FLOATEXCEP);
}
