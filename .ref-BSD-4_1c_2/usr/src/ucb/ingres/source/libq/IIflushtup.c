# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIflushtup.c	7.3	4/26/81)


/*
**	IIflushtup is called to syncronize the data pipe
**	after a retrieve.
*/

IIflushtup(file_name, line_no)
char	*file_name;
int	line_no;
{
	register int		i;

	if (IIproc_name = file_name)
		IIline_no = line_no;

#	ifdef xATR1
	if (IIdebug)
		printf("IIflushtup: IIerrflag %d\n", IIerrflag);
#	endif

	if (IIerrflag < 2000)
	{
		while ((IIpb.pb_stat & PB_EOF) == 0)
			IIpb_read(&IIpb);

		/* read the RESP block */
		IIpb_prime(&IIpb, PB_NOTYPE);
		IIreadinput(&IIpb);
	}

	IIin_retrieve = 0;
	IIndomains = 0;
	IIdomains = 0;
	IInewqry = 0;
}
