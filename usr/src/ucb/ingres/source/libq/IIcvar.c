# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIcvar.c	7.2	10/27/81)


/*
**	IIcvar -- write C variable values to parser
**
**
**		IIcvar is used to write the contents
**		of a C-variable to the quel parser.
**
**		Floats are converted to doubles first.
**
*/

IIcvar(obj, type, len)
char	*obj;
int	type;
int	len;
{
	register int		length;
	register ANYTYPE	*addr;
	int			t;
	double			d;

	t = type;	/* copy type of symbol */
	length = len;	/* and its length */
	addr = (ANYTYPE *) obj;	/* and a pointer to it */

	switch (t)
	{

	  case opFLOAT:
		/* convert from f4 to f8 */
		d = addr->f4type;
		addr = (ANYTYPE *) &d;
		length = sizeof d;
		t = opDOUBLE;
		break;

	  case opSTRING:
		length = IIlength(addr) + 1;	/* length includes null byte at end */

	  case opSHORT:
	  case opLONG:
	  case opDOUBLE:
		break;

	  default:
		IIsyserr("IIcvar:bad type %d", t);
	}


#	ifdef xETR1
	if (IIdebug)
		printf("IIcvar:type %d, length %d\n", t, length);
#	endif

	IIpb_put(&t, 1, &IIpb);
	IIpb_put(addr, length, &IIpb);
}
