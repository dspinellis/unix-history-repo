# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIsync.c	7.2	2/16/81)


/*
**	IIsync is called to syncronize the running
**	of a query with the running of the equel process.
**
**	The query is flushed and an EOP is written
**	to the quel parser.
**
**	The quel parser will write an end-of-pipe when
**	an operation is complete.
*/

IIsync(file_name, line_no)
char	*file_name;
int	line_no;
{
	pb_t	pb;

	IIpb_flush(&IIpb);
	if (IIproc_name = file_name)
		IIline_no = line_no;

#	ifdef xETR1
	if (IIdebug)
		printf("IIsync\n");
#	endif


	IIerrflag = 0;	/* reset error flag. If an error occures,
			** IIerrflag will get set in IIerror
			*/

	IIpb_prime(&pb, PB_NOTYPE);
	IIreadinput(&pb);
	IInewqry = 0;
}
