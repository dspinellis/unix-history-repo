# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	<sccs.h>

SCCSID(@(#)interp1.c	7.2	3/6/81)


/*
**	INTERP1.C
**
**	symbol I/O utility routines for OVQP interpreter.
**
*/

/*
** GETSYMBOL
**
**	Gets (type, len, value) symbols from list
**	A ptr to the list is advanced after
**	call.  Symbols are moved to a target location
**	(typically a slot on the interpreter's De.ov_stack).
**	Legality of symbol type and length is checked.
**	Returns		1  if no more symbols in list
**			0 otherwise
**
*/

getsymbol(ts, p)
SYMBOL	*ts;		/* target location (on stack) */
SYMBOL	***p;		/* pointer to list */
{
	int			len;	/*length of target symbol*/
	register union symvalue	*d;	/* ptr to data for target symbol */
	register SYMBOL		*cp;	/* the item in the list */
	register SYMBOL		*tops;	/* target location on stack */
	register union symvalue	*val;

	tops = ts;	/* copy stack pointer */
	cp = **p;	/* get list pointer */

#	ifdef xOTR1
	if (tTf(84, 0))
	{
		printf("GETSYM: ");
		prsym(cp);
	}
#	endif

	if (tops >= (SYMBOL *) &De.ov_stack[STACKSIZ])
		ov_err(STACKOVER);

	val = &cp->value;
	/* decomp will put the s_var's value in the right place 
	 * if this is the case
	 */
	if (cp->type == VAR || cp->type == S_VAR)
	{
		tops->type = val->sym_var.varfrmt;
		len = tops->len = val->sym_var.varfrml;
		d = (union symvalue *) val->sym_var.valptr;
	}
	else
	{
		tops->type = cp->type;
		len = tops->len = cp->len;
		len &= 0377;
		d = &cp->value;
	}
	/* advance De.ov_qvect sequencing pointer p */
	*p += 1;

	switch(tops->type)
	{
	  case INT:
		switch (len)
		{
		  case 1:
			tops->value.sym_data.i2type = d->sym_data.i1type;
			break;
		  case 2:
		  case 4:
			bmove((char *) d, (char *) &tops->value.sym_data, len);
			break;

		  default:
			syserr("getsym:bad int len %d",len);
		}
		break;

	  case FLOAT:
		switch (len)
		{
		  case 4:
			tops->value.sym_data.f8type = d->sym_data.f4type;
			break;

		  case 8:
			tops->value.sym_data.f8type = d->sym_data.f8type;
			break;

		  default:
			syserr("getsym:bad FLOAT len %d",len);
		}
		break;

	  case CHAR:
		tops->value.sym_data.cptype = d->sym_data.c0type;
		break;

	  case AOP:
	  case BOP:
	  case UOP:
	  case COP:
		tops->value.sym_op.opno = d->sym_op.opno;
		break;

	  case RESDOM:
		tops->value.sym_resdom.resno = d->sym_resdom.resno;
		break;

	  case AND:
	  case OR:
		break;

	  case AGHEAD:
	  case BYHEAD:
	  case ROOT:
	  case QLEND:
		return (1);	/* all these are delimitors between lists */

	  default:
		syserr("getsym:bad type %d", tops->type);
	}
	return(0);
}
/*
**  TOUT
**
**	Copies a symbol value into the Output tuple buffer.
** 	Used to write target
**	list elements or aggregate values into the output tuple.
*/

tout(s, offp, rlen)
register SYMBOL	*s;
char		*offp;
int		rlen;
{
	register int	i;
	register char	*p;
	int		slen;

#	ifdef xOTR1
	if (tTf(84, 3))
	{
		printf("TOUT: s=");
		prstack(s);
		printf("  offset=%d, rlen=%d\n", offp-De.ov_outtup, rlen);
	}
#	endif
	if (s->type == CHAR)
	{
		slen = s->len & 0377;
		rlen &= 0377;
		i = rlen - slen;	/* compute difference between sizes */
		if (i <= 0)
		{
			bmove(s->value.sym_data.cptype, offp, rlen);
		}
		else
		{
			p = s->value.sym_data.cptype;
			bmove(p, offp, slen);
			p = &offp[slen];
			while (i--)
				*p++ = ' ';	/* blank out remainder */
		}
	}
	else
	{
		bmove((char *)&s->value, offp, rlen);
	}
}
/*
**  RCVT
*/
rcvt(tos, restype, reslen)
register SYMBOL	*tos;
int		restype, reslen;
{
	register int	rtype, rlen;
	int		stype, slen;

	rtype = restype;
	rlen = reslen;
	stype = tos->type;
	slen= tos->len;
#	ifdef xOTR1
	if (tTf(84, 6))
	{
		printf("RCVT:type=");
		xputchar(rtype);
		printf("%3d, tos=", rlen);
		prstack(tos);
	}
#	endif

	if (rtype != stype)
	{
		if (rtype == CHAR || stype == CHAR)
			ov_err(BADCONV);	/* bad char to numeric conversion requested */
		if (rtype == FLOAT)
			itof(tos);
		else
		{
			if (rlen == 4)
				ftoi4(tos);
			else
				ftoi2(tos);
		}
		tos->len = rlen;	/* handles conversion to i1 or f4 */
	}

	else
	{
		if (rtype != CHAR && rlen != slen)
		{
			if (rtype == INT)
			{
				if (rlen == 4)
					i2toi4(tos);
				else if (slen == 4)
						i4toi2(tos);
			}
			tos->len = rlen;	/* handles conversion to i1 or f4 */
		}
	}
#	ifdef xOTR3
	if (tTf(84, 6))
	{
		printf("RCVT rets: symbol: ");
		prsym(tos);
	}
#	endif
}
