# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)out_arg.c	7.1	2/5/81)

/*
**  OUTPUT ARGUMENTS GLOBAL INITIALIZATION
*/

struct out_arg	Out_arg =		/* output arguments */
{
	6,		/* c0width */
	6,		/* i1width */
	6,		/* i2width */
	13,		/* i4width */
	10,		/* f4width */
	10,		/* f8width */
	3,		/* f4prec */
	3,		/* f8prec */
	'n',		/* f4style */
	'n',		/* f8style */
	66,		/* linesperpage */
	'|',		/* coldelim */
};
