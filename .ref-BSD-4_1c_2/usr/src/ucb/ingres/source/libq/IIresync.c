# include	<ingres.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIresync.c	7.1	2/5/81)


/*
**  RESYNCHRONIZE PIPES AFTER AN INTERRUPT
**
**	The pipes are all cleared out.  This routines must be called
**	by all processes in the system simultaneously.  It should be
**	called from the interrupt catching routine.
*/

int	IISyncs[CM_MAXPROC];
extern	exit();
int	(*IIinterrupt)() =	exit;

IIresync()
{
	register int	i;
	pb_t		pb;
	register int	stat;

	signal(2,1);

	/*
	**  Send SYNC blocks to all processes that are adjacent
	**	in the write direction.
	**  Arrange to ignore blocks from all processes that
	**	are adjacent in the read direction.
	*/

	IIpb_prime(&pb, PB_SYNC);
	for (i = 0; i < CM_MAXPROC; i++)
	{
		IISyncs[i]++;

		/* send SYNC to parser */
		pb.pb_proc = 1;
		IIpb_write(&pb);
	}

	/* ovqp buffer flush is done in IIsetup() */

	/* Get out of a retrieve and clear errors */
	IIin_retrieve = 0;
	IIerrflag = 0;
	IIndomains = IIdomains = 0;
	IInewqry = 0;


	/* reset the signal */
	signal(2, IIresync);
	/* allow the user to service the interrupt */
	(*IIinterrupt)(-1);
	/*
	** If IIinterupt returns the user might hang in a retrieve
	*/

	IIsyserr("Interupt returned");
}

