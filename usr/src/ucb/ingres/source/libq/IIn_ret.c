# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIn_ret.c	7.2	10/27/81)



/*
**  IIn_ret -- get next domain in a retrieve
**
**	Gets the next domain in a retrieve from the data
**	pipe. If an error occurred previously in the tuple,
**	will not load the c-var with the value of the domain.
**	Performs the conversion from the gotten type to
**	the expected type.
**
**	Signals any errors and calls IIerror() accordingly.
**
**	Expects the type and length of the next data item in
**	IIr_sym.
**
*/




IIn_ret(addr, type)
char	*addr;
int	type;
{
	char			temp[256], *IIitos();
	char			*s;
	register struct retsym	*ret;
	register		length;



	if (IIerrflag && IIerrflag != 1001)
		return;		/* error, no data will be coming, or
				 * the rest of the query should be
				 * skipped
				 */

        ret = &IIr_sym;

	if (ret->len &= 0377)
		if (IIpb_get(&IIpb, temp, ret->len & 0377) != ret->len & 0377)
			IIsyserr("IIn_ret: bad pb_get-1 %d", ret->len & 0377);


#	ifdef xETR1
	if (IIdebug)
	{
		printf("%s ent ", IIproc_name ? IIproc_name: "");
		printf("IIn_ret : addr 0%o type %d length %d type %d IIerrflag %d\n",
		addr, type, ret->len & 0377, ret->type, IIerrflag);
	}
#	endif


	IIdomains++;
	switch (type)
	{

	  case opSHORT:
		type = INT;
		length = 2;
		break;

	  case opLONG:
		type = INT;
		length = 4;
		break;

	  case opFLOAT:
		type = FLOAT;
		length = 4;
		break;

	  case opDOUBLE:
		type = FLOAT;
		length = 8;
		break;

	  case opSTRING:
		type = CHAR;
		length = 255;	/* with the current implementation the length is not known */
		break;

	  default:
		IIsyserr("IIn_ret:bad type %d", type);
	}

	switch (ret->type)
	{

	  case INT:
	  case FLOAT:
		if (type == CHAR)
		{
			s = IIitos(IIdomains);
			IIerrflag = 1000;
			IIerror(1000, 1, &s);
			return (0);
		}
		if (IIconvert(temp, IIerrflag ? temp : addr,
		   ret->type, ret->len & 0377, type, length) < 0)
		{
				s = IIitos(IIdomains);
				IIerrflag = 1001;
				IIerror(1001, 1, &s);
		}
		break;

	  case CHAR:
		if (type != CHAR)
		{
			s = IIitos(IIdomains);
			IIerrflag = 1002;
			IIerror(1002, 1, &s);
			return (0);
		}
		if (!IIerrflag)
		{
			IIbmove(temp, addr, ret->len & 0377);

			/* null terminate string */
			addr [ret->len & 0377] = '\0';	
		}
		break;

	  default :
		IIsyserr("IIn_ret bad gotten type %d",
		ret->type);
	}

	if (IIpb_get(&IIpb, ret, 2) != 2)
		IIsyserr("IIn_ret : bad pb_get - 2");
}
