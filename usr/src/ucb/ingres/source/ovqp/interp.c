# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	<access.h>
# include	"../decomp/globs.h"
# include	<sccs.h>

SCCSID(@(#)interp.c	7.3	3/6/81)



/*
**
** INTERPRET
**
**	 Processes the retrieved tuple from the De.ov_source relation
**	 according to the symbols in the list.  Recognition
**	 of characteristic delimiters and separators initiates
**	 action appropriate to a target list or qualification list
**	 as the case may be.
**	
**	 Between delimiters, the symbol list is expected to be in
**	 Polish postfix form.  A qualification list is further
**	 expected to be in conjunctive normal form, with Boolean
**	 operators infixed.
**
*/


double	pow();
double	sqrt();
double	log();
double	exp();
double	sin();
double	cos();
double	atan();

# define SPUSH(v)	(++((struct stacksym *) v))
# define SPOP(v)	(((struct stacksym *) v)--)

SYMBOL *
interpret(list)
SYMBOL	**list;	/* ptr to list of sym pointers */
{
	register SYMBOL		*tos;
	SYMBOL			*op1,*op2;	/*operands popped off stack*/
	register ANYTYPE	*val1,*val2;	/*ptrs to values of operands*/
	int			opval, optype, l1;
	char			*s1;
	int			byflag;
	long			hitid;
	extern char		*Usercode;
	extern			ov_err();
	int			cb_mark;
	extern char		*ov_ovqpbuf;
	extern char		*locv();

#	ifdef xOTR1
	if (tTf(72, 0))
		printf("INTERP:  list=%x\n",list);
#	endif

#	ifdef xOTM
	if (tTf(90, 2))
		timtrace(11, 0);
#	endif

	byflag = (list == De.ov_bylist);	/* set byflag if aggregate function */
	tos = (SYMBOL *)(De.ov_stack-1);
	/* reset the concat and ascii operator buffer */
	seterr(De.ov_ovqpbuf, CBUFULL, ov_err);
	cb_mark = markbuf(De.ov_ovqpbuf);

loop:
#	ifdef xOTR1
	if (tTf(72, 1) && tos >= (SYMBOL *) De.ov_stack)
	{
		printf("\ttops of stack=");
		prstack(tos);	/* print top of stack element */
	}
#	endif
	/* check for stack overflow */
	l1 = getsymbol(SPUSH(tos), &list);

	if (l1)
	{
#		ifdef xOTM
		if (tTf(90, 2))
			timtrace(12, 0);
#		endif

		freebuf(De.ov_ovqpbuf, cb_mark);
		return (tos);
	}
	optype = tos->type;
	opval = tos->value.sym_data.i2type;
	SPOP(tos);		/* assume that stack will be popped */

	switch(optype)
	{
	  case INT:
	  case FLOAT:
	  case CHAR:
		SPUSH(tos);		/* just leave symbol on stack */
		goto loop;

	  case COP:
		SPUSH(tos);		/* new symbol goes on stack */
		tos->type = CHAR;
		switch (opval)
		{

		  case opDBA:
			tos->value.sym_data.cptype = Admin.adhdr.adowner;
			tos->len = 2;
			goto loop;

		  case opUSERCODE:
			tos->value.sym_data.cptype = Usercode;
			tos->len = 2;
			goto loop;
		}

	  case AND:		/* if top value false return immediately */
		if (!tos->value.sym_data.i2type)
		{
#			ifdef xOTM
			if (tTf(90, 2))
				timtrace(12, 0);
#			endif
			freebuf(De.ov_ovqpbuf, cb_mark);
			return(tos);
		}
		else
			SPOP(tos);
		freebuf(De.ov_ovqpbuf, cb_mark);
		goto loop;

	  case OR:		/* if top value is true then skip to
				** end of disjunction. */
		if (tos->value.sym_data.i2type)
		{
			SPUSH(tos);
			do
			{
				getsymbol(tos, &list);
			} while (tos->type != AND);
			optype = AND;
			SPOP(tos);
		}
		SPOP(tos);
		goto loop;

	  case RESDOM:
		freebuf(De.ov_ovqpbuf, cb_mark); /* init the concat and ascii buffer */
		if (De.ov_result)
		{
			if (opval)	/* if gt zero then opval represents a real domain */
			{
				if (byflag)
					opval++;	/* skip over count field for ag functs */
				rcvt(tos, De.ov_result->relfrmt[opval], De.ov_result->relfrml[opval]);
				tout(tos, De.ov_outtup+De.ov_result->reloff[opval], De.ov_result->relfrml[opval]);
			}
			else	/* opval refers to the tid and this is an update */
			{
				De.ov_uptid = tos->value.sym_data.i4type;	/* copy tid */
				if (De.de_qmode == mdREPL || (De.ov_diffrel && De.de_qmode == mdDEL && De.ov_result->reldum.relindxd > 0 ))
				{
					/* De.ov_origtup must be left with the orig
					** unaltered tuple, and De.ov_outtup must
					** be initialized with the orig tuple.
					**
					** De.ov_outtup only matters with REPL.
					** Scan() sets up De.ov_origtup so when
					** De.ov_source == De.ov_result, origtup is already
					** correct.
					*/

					if (De.ov_diffrel)
					{
						if (l1 = get(De.ov_result, &De.ov_uptid, &hitid, De.ov_origtup, CURTUP))
							syserr("interp:get on resdom %s, %d", locv(De.ov_uptid), l1);
						bmove(De.ov_origtup, De.ov_outtup, De.ov_result->reldum.relwid);
					}
					else
					{
						bmove(De.ov_intup, De.ov_outtup, De.ov_result->reldum.relwid);
					}
				}
			}
		}
		else
		{
			if (Equel)
				equelatt(tos);	/* send attribute to equel */
			else
			{
				if (tos->type == CHAR)
					s1 = tos->value.sym_data.cptype;
				else
					s1 = tos->value.sym_data.c0type;
				printatt(tos->type, tos->len & 0377, s1);	/* print attribute */
			}
		}
		SPOP(tos);
		goto loop;


	  case BOP:
		op2 = (SYMBOL *)SPOP(tos);
		op1 = (SYMBOL *)tos;
		typecheck(op1, op2, opval);
		val1 = &op1->value.sym_data;
		val2 = &op2->value.sym_data;

		switch (tos->type)
		{
		  case INT:
			switch (tos->len)
			{
			  case 1:
			  case 2:
				switch (opval)
				{
				  case opADD:
					val1->i2type += val2->i2type;
					goto loop;

				  case opSUB:
					val1->i2type -= val2->i2type;
					goto loop;

				  case opMUL:
					val1->i2type *= val2->i2type;
					goto loop;

				  case opDIV:
					val1->i2type /= val2->i2type;
					goto loop;

				  case opMOD:
					val1->i2type %= val2->i2type;
					goto loop;

				  case opPOW:
					itof(op1);
					itof(op2);
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  /* relational operator */
				  default:
					l1 = val1->i2type - val2->i2type;
					val1->i2type = relop_interp(opval, l1);
					goto loop;
				}

			  case 4:
				switch(opval)
				{
				  case opADD:
					val1->i4type += val2->i4type;
					goto loop;

				  case opSUB:
					val1->i4type -= val2->i4type;
					goto loop;

				  case opMUL:
					val1->i4type *= val2->i4type;
					goto loop;

				  case opDIV:
					val1->i4type /= val2->i4type;
					goto loop;

				  case opMOD:
					val1->i4type %= val2->i4type;
					goto loop;

				  case opPOW:
					itof(op1);
					itof(op2);
					val1->f8type = pow(val1->f8type, val2->f8type);
					goto loop;

				  /* relational operator */
				  default: 
					tos->len = 2;
					if (val1->i4type > val2->i4type)
						l1 = 1;
					else
						if (val1->i4type == val2->i4type)
							l1 = 0;
						else
							l1 = -1;

					val1->i2type = relop_interp(opval, l1);
					goto loop;

				}
			}

		  case FLOAT:
			switch (opval)
			{
			  case opADD:
				val1->f8type += val2->f8type;
				goto loop;

			  case opSUB:
				val1->f8type -= val2->f8type;
				goto loop;

			  case opMUL:
				val1->f8type *= val2->f8type;
				goto loop;

			  case opDIV:
				val1->f8type /= val2->f8type;
				goto loop;

			  case opPOW:
				val1->f8type = pow(val1->f8type, val2->f8type);
				goto loop;

			  default:
				tos->type = INT;
				tos->len = 2;
				if (val1->f8type > val2->f8type)
					l1 = 1;
				else
					if (val1->f8type == val2->f8type)
						l1 = 0;
					else
						l1 = -1;
				val1->i2type = relop_interp(opval, l1);
				goto loop;
			}

		case CHAR:
			if (opval == opCONCAT)
			{
				concatsym(op1, op2);	/* concatenate the two symbols */
				goto loop;
			}
			l1 = lexcomp(val1->cptype, op1->len & 0377, val2->cptype, op2->len & 0377);
			tos->type = INT;
			tos->len = 2;
			val1->i2type = relop_interp(opval, l1);
			goto loop;
		}	/* end of BOP */
 
	   case UOP:
		val1 = &tos->value.sym_data;
		switch (opval)
		{
		   case opMINUS:
		   case opABS:
			if (tos->type == CHAR)
				ov_err(BADUOPC);
			l1 = opval == opMINUS;
			switch (tos->type)
			{
			   case INT:
				switch (tos->len)
				{
				  case 1:
				  case 2:
					if (l1 || val1->i2type < 0)
						val1->i2type = -val1->i2type;
			   		goto loop;

				  case 4:
					if (l1 || val1->i4type < 0)
						val1->i4type = -val1->i4type;
					goto loop;
				}

			  case FLOAT:
				if (l1 || val1->f8type < 0.0)
					val1->f8type = -val1->f8type;
				goto loop;
			}

		  case opNOT:
			val1->i2type = !val1->i2type;
		  case opPLUS:
			if (tos->type == CHAR)
				ov_err(BADUOPC);
			goto loop;

		  case opASCII:
			ascii(tos);
			goto loop;

		  case opINT1:
			typecoerce(tos, INT, 1);
			goto loop;

		  case opINT2:
			typecoerce(tos, INT, 2);
			goto loop;

		  case opINT4:
			typecoerce(tos, INT, 4);
			goto loop;

		  case opFLOAT4:
			typecoerce(tos, FLOAT, 4);
			goto loop;

		  case opFLOAT8:
			typecoerce(tos, FLOAT, 8);
			goto loop;

		  default:
			if (tos->type == CHAR)
				ov_err(BADUOPC);
			if (tos->type == INT)
				itof(tos);
			switch (opval)
			{
			  case opATAN:
				val1->f8type = atan(val1->f8type);
				goto loop;
	
			  case opLOG:
				val1->f8type = log(val1->f8type);
				goto loop;
	
			  case opSIN:
				val1->f8type = sin(val1->f8type);
				goto loop;

			  case opCOS:
				val1->f8type = cos(val1->f8type);
				goto loop;

			  case opSQRT:
				val1->f8type = sqrt(val1->f8type);
				goto loop;

			  case opEXP:
				val1->f8type = exp(val1->f8type);
				goto loop;

			  default:
				syserr("interp:bad uop %d",opval);
			}
		}


	   case AOP:
		aop_interp(opval, tos);
		SPOP(tos);		/* pop this symbol */
		goto loop;

	}
	syserr("interp: fell out");
	/*NOTREACHED*/
}
/*
**	relop_interp interprets the relational operators
**	(ie. EQ, NE etc.) and returns true or false
**	by evaluating l1.
**
**	l1 should be greater than, equal or less than zero.
*/

relop_interp(opval, l1)
int	opval;
int	l1;
{
	register int	i;

	i = l1;

	switch (opval)
	{

	  case opEQ:
		return (i == 0);

	  case opNE:
		return (i != 0);

	  case opLT:
		return (i < 0);

	  case opLE:
		return (i <= 0);

	  case opGT:
		return (i > 0);

	  case opGE:
		return (i >= 0);

	  default:
		syserr("relop:bad relop or bop %d", opval);
	}
	/*NOTREACHED*/
}
/*
**	Aggregate values are stored in De.ov_outtup. De.ov_tend points
**	to the spot for the next aggregate. Aop_interp()
**	computes the value for the aggregate and leaves
**	the result in the position pointed to by De.ov_tend.
*/

aop_interp(opval, tos)
int		opval;
register SYMBOL	*tos;
{
	register int	i;
	int		l1;
	ANYTYPE		numb;	/* used for type conversion */

	bmove(De.ov_tend, (char *) &numb, 8);	/* note: this assumes that there are always 8 bytes which can be moved.
				** if it moves beyond De.ov_tend, it's ok */
	switch (opval)
	{

	  case opSUMU:
	  case opSUM:
		if (*De.ov_counter <= 1)
			goto puta;
		switch (tos->type)
		{
		  case INT:
			switch(tos->len)
			{
			  case 1:
				tos->value.sym_data.i2type += numb.i1type;
				goto puta;

			  case 2:
				tos->value.sym_data.i2type += numb.i2type;
				goto puta;

			  case 4:
				tos->value.sym_data.i4type += numb.i4type;
				goto puta;
			}

		  case FLOAT:
			if (tos->len == 4)
				numb.f8type = numb.f4type;
			tos->value.sym_data.f8type += numb.f8type;
			goto puta;

		  default:
			ov_err(BADSUMC);	/* cant sum char fields */
		}

	  case opCOUNTU:
	  case opCOUNT:
		tos->type = CNTTYPE;
		tos->len = CNTLEN;
		tos->value.sym_data.i4type = *De.ov_counter;
		goto puta;

	  case opANY:
		tos->type = OANYTYPE;
		tos->len = OANYLEN;
		if (*De.ov_counter)
		{
			tos->value.sym_data.i2type = 1;
			if (!De.ov_bylist && (De.ov_agcount == 1))
				De.ov_targvc = 0;	/* if simple agg. stop scan */
		}
		else
			tos->value.sym_data.i2type = 0;
		goto puta;

	  case opMIN:
	  case opMAX:
		if (*De.ov_counter<=1)
			goto puta;
		switch (tos->type)
		{
		  case INT:
			switch (tos->len)
			{
			  case 1:
				i = (tos->value.sym_data.i1type < numb.i1type);
				break;

			  case 2:
				i = (tos->value.sym_data.i2type < numb.i2type);
				break;

			  case 4:
				i = (tos->value.sym_data.i4type < numb.i4type);
				break;
			}
			break;

		  case FLOAT:
			if (tos->len == 4)
				numb.f8type = numb.f4type;
			i = (tos->value.sym_data.f8type < numb.f8type);
			break;

		  case CHAR:
			l1 = tos->len & 0377;
			i = (lexcomp(tos->value.sym_data.cptype, l1, De.ov_tend, l1) < 0);
			break;

		  default:	
			syserr("interp:bad op type for opMIN/MAX %d", tos->type);
		}

		/* check result of comparison */
		if (opval == opMAX)
			i = !i;	/* complement test for opMAX */
		if (i)
			goto puta;	/* condition true. new value */

		/* condition false. Keep old value */
		goto done;


	  case opAVGU:
	  case opAVG:
		if (tos->type == INT)
			itof(tos);
		else
			if (tos->type == CHAR)
				ov_err(BADAVG);
		if (*De.ov_counter > 1)
		{
			tos->value.sym_data.f8type = numb.f8type + (tos->value.sym_data.f8type - numb.f8type) / *De.ov_counter;
		}
		tos->len = 8;
		goto puta;

	  default:
		syserr("interp:bad agg op %d", tos->type);
	}

puta:
	tout(tos, De.ov_tend, tos->len);
done:
	De.ov_tend += tos->len & 0377;
}
