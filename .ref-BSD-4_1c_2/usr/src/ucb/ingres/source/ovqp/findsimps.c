# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	"strategy.h"
# include	<sccs.h>

SCCSID(@(#)findsimps.c	7.1	2/5/81)

/*
**	Findsimps searches the qualification for
**	occurances of simple clauses. In its
**	current crude implementation it only finds
**	cluases of the form:
**
**	var relop constant  or  constant relop var
**
**	it does not use simple clauses with 'OR's
**	nor does it accept clauses of the form
**
**	var relop constant + constant    etc.
**
**	Findsimps knows about pattern matching characters
**	and treats char constants containing pm chars
**	specially. For example
**		var >= "xx*"  --> var >= "xx"
**		var <= "xx*"  --> var <= "xx\0177"
**		var =  "xx*"  --> var >= "xx" and var <= "xx\0177"
**	If the first char is a pm char then the clause is not
**	considered as a simple clause. Also notice that the conversion
**	is done only once. If the next time De.ov_newq = FALSE, then findsimps()
**	isn't called. This works because a pm char can only come from
**	the user and not from a relation. Thus during tuple substition
**	a constant with a pm will never change.
*/

findsimps()
{
	register struct symbol	*c;
	register int		t;
	int			length;
	register struct symbol	**q;
	int			attno, rel, found;
	struct symbol		*cpsym(), *xc;

#	ifdef xOTR1
	if (tTf(81, 0))
		printf("FINDSIMPS\n");
#	endif
	De.ov_nsimp = 0;
	found = FALSE;
	q = De.ov_qlist;	/* q holds pointer to qualification */

	if (!q)
		return (0);


	for (t = (*q)->type; t != QLEND; t = (*++q)->type)
	{
		switch (t)
		{
		  case VAR:
			attno = (*q)->value.sym_var.attno;	/* save att number */
			t = (*++q)->type;
			if (t == INT || t == FLOAT || t == CHAR || t == S_VAR)
			{
				c = *q;	/* save pointer to value symbol */
				t = (*++q)->type;
				if ((rel = relop(*q, FALSE)) >= 0 
				   && (t = (*++q)->type) == AND)
				{
					/* found a simple clause */
					found = TRUE;
				}
			}
			break;

		  case S_VAR:
		  case INT:
		  case FLOAT:
		  case CHAR:
			c = *q++;
			if ((t = (*q)->type) == VAR)
			{
				attno = (*q)->value.sym_var.attno;
				t = (*++q)->type;
				if ((rel = relop(*q, TRUE)) >= 0 && (t = (*++q)->type) == AND)
				{
					/* found a simple clause */
					found = TRUE;
				}
			}
		}
		if (found)
		{
			/* a simple clause has been found.
			** Check that the constant contains
			** at least one char before any pattern
			** matching char. If there is a pattern
			** matching char then special processing
			** must be done.
			*/

			found = FALSE;
			if (length = check(c))
			{

				/*
				** If length is zero then the first char was
				** a pattern matching char. If length < 0 then
				** no pattern matching char, and finally
				** if length > 0 then length is the number of
				** chars before the first pattern matching char
				*/
				if (length > 0)
				{
					switch (rel)
					{

					  case opEQ:
						/*
						** Create two simple clauses:
						** One below the value and the
						** other above the value.
						*/
						xc = cpsym(c, length, opLTLE);
						add_simp(xc, opLTLE, attno);
						rel = opGTGE;
						/* fall through to GTGE case */

					  case opGTGE:
						c = cpsym(c, length, opGTGE);
						break;

					  case opLTLE:
						c = cpsym(c, length, opLTLE);
						break;
					}
				}

				if (add_simp(c, rel, attno))
					break;	/* no more room in simps */
			}
		}
		while (t != AND)	/* skip to next AND */
			t = (*++q)->type & 0377;
	}
#	ifdef xOTR1
	if (tTf(81, 2))
		printf("findsimps returning %d\n", De.ov_nsimp);
#	endif
	return (De.ov_nsimp);
}


relop(s, reverse)
struct symbol	*s;
int		reverse;

/*
**	relop determines whether a symbol is a
**	usable relational operator ie. =,>,>=,<,<=
**
**	returns the type of the relational
**	operator if found, else it returns
**	-1
**
**	Items are normalized to be in the form:
**	var relop constant. If reverse is TRUE then
**	complement the sense of the relop. Reverse will
**	be TRUE is the simple clause was found in the
**	form constant relop var.
*/

{
	register int	v;

	v = -1;	/* assume failure */
	if (s->type == BOP)
	{
		switch (s->value.sym_op.opno)
		{

		  case opEQ:
			v = opEQ;
			break;

		  case opLT:
		  case opLE:
			v = opLTLE;
			if (reverse)
				v = opGTGE;
			break;

		  case opGT:
		  case opGE:
			v = opGTGE;
			if (reverse)
				v = opLTLE;
			break;

		}
	}
	return (v);
}



check(sym)
struct symbol	*sym;

/*
**	check checks the symbol for
**	pattern matching characters.
**	If any are found then check returns
**	the number of characters before the
**	first pattern matching character.
**
**	If no pattern matching chars are found
**	then check returns -1.
**
**	note that PAT_RBRAC need not be checked for
**	since it is not a pattern matching char unless
**	PAT_LBRAC appears before it.
**
**	PAT_LBRAC is treated specially in cpsym().
**	If any are detected, then length until the
**	first PAT_ANY or PAT_ONE is returned.
*/

{
	register struct symbol	*s;
	register char		*cp;
	register int		len;
	int			flag;

	s = sym;
#	ifdef xOTR1
	if (tTf(81, 4))
	{
		printf("Checksym:");
		prsym(s);
	}
#	endif
	if (s->type == CHAR)
	{
		flag = FALSE;
		cp = s->value.sym_data.c0type;	/* the string is a literal */
		len = s->len & 0377;
		while (len--)
		{
			switch(*cp++)
			{

			  case PAT_ANY:
			  case PAT_ONE:
				return ((s->len & 0377) - len - 1);

			  case PAT_LBRAC:
				flag = TRUE;

			}
		}
		if (flag)
			return (s->len & 0377);	/* constant had PAT_LBRAC char */
	}
	return (-1);	/* ok */
}


struct symbol 
*cpsym(const, len, op)
struct symbol	*const;
int		len;
int		op;

/*
** Cpsym -- copy a symbol to a new buffer area.
**	If op is opLTLE then add a pad character
**	whose value is the largest possible char
**	value.
**
**	If any ranges of characters are found,
**	then the lowest/highest char is taken from
**	range.
*/

{
	register struct symbol	*s;
	register char		*cp;
	register int		i;
	char			*sp, c, nc;
	extern char		*ov_ovqpbuf;
	char			*need();

	i = len;
	s = (struct symbol *)need(De.ov_ovqpbuf, op == opLTLE ? i + 3 : i + 2);
	s->type = CHAR;
	sp = s->value.sym_data.c0type;
	cp = const->value.sym_data.c0type;

	while (i--)
	{
		/* copy chars processing LBRAC chars if any */
		if ((c = *cp++) == PAT_LBRAC)
		{
			/* if string is empty, ignore it */
			if (i == 0)
				break;

			c = *cp++;
			i--;

			if (c == PAT_RBRAC)
				continue;	/* empty [] */

			while (i-- && ((nc = *cp++) != PAT_RBRAC))
			{
				/* ignore '-' */
				if (nc == '-')
					continue;

				/* check for char larger/smaller than 'c' */
				if (op == opLTLE)
				{
					if (nc > c)
						c = nc;
				}
				else
				{
					if (nc < c)
						c = nc;
				}
			}
		}

		*sp++ = c;	/* copy next char */
	}
	if (op == opLTLE)
		*sp++ = 0177;
	s->len = sp - s->value.sym_data.c0type;

	return (s);
}


add_simp(const, rel, attno)
struct symbol	*const;
int		rel;
int		attno;

/*
** Add_simp -- add a simple clause to the list of
**	simple clauses. As a side effect the De.ov_nsimp
**	is incremented. If there is no room return
**	TRUE else return FALSE
*/

{
	register struct simp	*s;

	if (De.ov_nsimp == NSIMP)
		return (TRUE);	/* no more room */

	s = &De.ov_simp[De.ov_nsimp++];

	s->att = attno;
	s->const = const;
	s->relop = rel;

#	ifdef xOTR1
	if (tTf(81, 3))
		prsimp(s);
#	endif

	return (FALSE);
}


prsimp(ss)
struct simp	*ss;
{
#	ifdef xOTR1
	struct simp	*s;

	s = ss;
	printf("simp:relop=%d,att=%d,val=", s->relop, s->att);
	prsym(s->const);
#	endif
}
