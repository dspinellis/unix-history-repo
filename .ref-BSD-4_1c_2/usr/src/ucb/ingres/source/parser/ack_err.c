# include	<ingres.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)ack_err.c	7.1	2/5/81)

/*
**  ACK_ERR -- the error passing routine for the parser
**
**	Trace Flags:
**		ack_err ~~ 65
*/

ack_err()
{
	extern int	Ingerr;
	extern int	Err_fnd;

# ifdef	xPTR1
	tTfp(65, 0, "ack_err\n");
# endif

	Ingerr = 1;

	Err_fnd += 1;

	return (1);
}
