# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	<sccs.h>

SCCSID(@(#)prsym.c	7.2	3/6/81)

prsym(s)
register SYMBOL	*s;
{
	register union symvalue	*p;
	register int		type;
	register int		len;
	union symvalue		temp;

	type = s->type;
	len = s->len & 0377;
	p = &s->value;
	if (type == S_VAR)
	{
		/* actually, S_VAR's are rendered the same as VAR's
		 * by call_ovqp70.c's ovqpnod()
		 */
		printf("s_");	/* first part of "s_var" message */
		type = VAR;	/* execute var portion */
	}
	if (type == VAR)
	{
		printf("var:att#=%d:", p->sym_var.attno);
		type = p->sym_var.varfrmt;
		len = p->sym_var.varfrml;
		if (type != CHAR)
		{
			/* move anytype to symvalue boundary */
			bmove((char *)p->sym_var.valptr, (char *)&temp, sizeof *p);
			p = &temp;
		}
	}
	xputchar(type);
	printf("%d:value='", len);
	switch (type)
	{
	  case AND:
	  case AOP:
	  case BOP:
	  case OR:
	  case RESDOM:
	  case UOP:
	  case COP:
		printf("%d (operator)", p->sym_op.opno);
		break;

	  case INT:
		switch (len)
		{
		  case 1:
			printf("%d", p->sym_data.i1type);
			break;

		  case 2:
			printf("%d", p->sym_data.i2type);
			break;

		  case 4:
			printf("%ld", p->sym_data.i4type);
		}
		break;

	  case FLOAT:
		printf("%10.3f", p->sym_data.f4type);
		break;

	  case RESULTID:
	  case SOURCEID:
	  case CHAR:
		printf("%x=", p->sym_data.c0type);
		prstr(p->sym_data.c0type, len);
		break;

	  case AGHEAD:
	  case BYHEAD:
	  case QLEND:
	  case ROOT:
	  case TREE:
		printf(" (delim)");
		break;

	  case CHANGESTRAT:
	  case REOPENRES:
	  case EXIT:
	  case QMODE:
	  case RETVAL:
	  case USERQRY:
		if (len)
			printf("%d", p->sym_op.opno);
		printf(" (status)");
		break;

	  default:
		printf("\nError in prsym: bad type= %d\n", type);
	}
	printf("'\n");
}


prstack(s)
register SYMBOL	*s;
{
	if (s->type == CHAR)
	{
		printf("c%d:value='%x=", s->len,s->value.sym_data.cptype);
		prstr(s->value.sym_data.cptype, s->len & 0377);
		printf("'\n");
	}
	else
		prsym(s);
}



prstr(p, l)
register char	*p;
register int	l;
{
	while (--l >= 0)
		putchar(*p++);
}
