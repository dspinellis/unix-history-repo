# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	<sccs.h>

SCCSID(@(#)comment.c	7.1	2/5/81)


/*
**  COMMENT.C -- routine to scan comments inside an equel statement
**
**	Uses the endcmnt token code to find the endcmnt
**	terminal string, then reads the input until it sees this 
**	terminal (must be <= 2 characters), returning EOF_TOK and
**	giving an error diagnostic if end-of-file is encountered.
**
**	Parameters:
**		none
**
**	Returns:
**		CONTINUE -- valid comment
**		EOF_TOK -- EOF in comment
**
**	Side Effects:
**		deletes comments from within an equel statement
*/


comment()
{
	register int		i, l;
	register struct optab	*op;
	char			buf [3];

	/* find end of comment operator */
	for (op = Optab; op->op_term; op++)
		if (op->op_token == Tokens.sp_endcmnt)
			break;

	if (!op->op_term)
		syserr("no end of comment operator in the parse tables");
	/* scan for the end of comment */
	l = length(op->op_term);
	if (l > sizeof buf - 1)
		syserr("comment : buf too short for endcmnt %s %d",
		op->op_term, l);

	/* fill buffer to length of endmnt terminal */
	for (i = 0; i < l; i++)		
	{
		if ((buf [i] = getch()) == EOF_TOK)
		{
nontermcom :
			/* non-terminated comment */
			yysemerr("premature EOF encountered in comment", 0);
			return (EOF_TOK);
		}
	}

	/* shift on input until endcmnt */
	while (!bequal(buf, op->op_term, l))
	{
		for (i = 0; i < l - 1; i++)
			buf [i] = buf [i + 1];
		if ((buf [l - 1] = getch()) == EOF_TOK)
			goto nontermcom;
	}
	return (CONTINUE);
}
