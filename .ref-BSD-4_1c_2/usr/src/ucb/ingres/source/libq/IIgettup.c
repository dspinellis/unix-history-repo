# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIgettup.c	7.1	2/5/81)


/*
**	IIgettup is called to retrieve one instance
**	of the target list into the c-variables.
**
**	Integers and Floating point numbers can be converted
**	to other numbers.
**
**	Characters fields must match other character fields.
*/

IIgettup(file_name, line_no)
char	*file_name;
int	line_no;
{
	register int		length, domain;
	register struct retsym	*ret;
	struct retsym		sym;
	char			temp[256], *IIitos(), *s;



	if (IIproc_name = file_name)
		IIline_no = line_no;

	if (IIerrflag)
		return (0);	/* error. no data will be coming */

	ret = IIretsym;
	domain = 0;

	for (;;)
	{
		if (IIpb_get(&IIpb, &sym, 2) != 2)
			IIsyserr("IIgettup bad rdpipe 1");
		if (length = sym.len & 0377)
			if (IIpb_get(&IIpb, temp, length) != length)
				IIsyserr("IIgettup bad rdpipe-2 %d", length);
#		ifdef xETR1
		if (IIdebug)
		{
			printf("%s ent ", IIproc_name ? IIproc_name : "");
			printf("gettup type %d len %d\n", sym.type, length);
		}
#		endif
		domain++;
		switch (sym.type)
		{

		  case INT:
		  case FLOAT:
			if (ret->type == CHAR)
			{
				s = IIitos(domain);
				IIerror(1000, 1, &s);
				return (0);
			}
			if (IIconvert(temp, ret->addr, sym.type, length, ret->type, ret->len & 0377) < 0)
			{
					s = IIitos(domain);
					IIerror(1001, 1, &s);
			}
			break;

		  case CHAR:
			if (ret->type != CHAR)
			{
				s = IIitos(domain);
				IIerror(1002, 1, &s);
				return (0);
			}
			IIbmove(temp, ret->addr, length);
			ret->addr[length] = '\0';	/* null terminate string */
			break;

		  case EOTUP:
			return (1);

		  case EXIT:
			return (0);
		}
		ret++;
	}
}
