# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIretrieve.c	7.2	10/27/81)


/*
**	IIretrieve is called once for each element
**	in the target list of a retrieve.
**
**	The purpose is to set up the IIretsym structure
**	for IIgettup.
*/

IIretrieve(addr, type)
char	*addr;
int	type;

{
	register struct retsym	*sym;
	register int		t, l;

	sym = &IIretsym[IIndomains++];
	switch (type)
	{

	  case opSHORT:
		t = INT;
		l = 2;
		break;

	  case opLONG:
		t = INT;
		l = 4;
		break;

	  case opFLOAT:
		t = FLOAT;
		l = 4;
		break;

	  case opDOUBLE:
		t = FLOAT;
		l = 8;
		break;

	  case opSTRING:
		t = CHAR;
		l = 255;	/* with the current implementation the length is not known */
		break;

	  default:
		IIsyserr("retrieve:bad type %d", type);
	}
	sym->type = t;
	sym->len = l;
	sym->addr = addr;
#	ifdef xETR1
	if (IIdebug)
		printf("domain %d type %d len %d\n", IIndomains, t, l);
#	endif
}
