# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	"y.tab.h"
# include	<sccs.h>

SCCSID(@(#)operator.c	7.1	2/5/81)


/*
**  OPERATOR -- process a token starting with an operator
**
**	Processes operators, strings, comments, and 
**	floating constants without a leading 0.
**
**	Parameters:
**		chr - first character of token {Cmap [chr] == OPATR}
**
**	Returns:
**		NUMBER or STRING token, or operator token.
**		CONTINUE on error.
**
**	Side Effects:
**		Adds a node to the Symbol space, and returns adress
**		in "yylval".
**		Opcode is set to the opcode of the operator.
**		May backup a character.
*/

operator(chr)
char	chr;
{
	register struct optab	*op;
	char			opbuf [3];

	opbuf [0] = chr;
	opbuf [1] = getch();
	opbuf [2] = '\0';

	if (opbuf [0] == '.' && Cmap [opbuf [1]] == NUMBR)
	{
		/* floating mantissa w/o leading 0 */
		backup(opbuf [1]);
		return (number(opbuf [0]));
	}
	if (Cmap [opbuf [1]] != OPATR)
	{
		backup(opbuf [1]);
		opbuf [1] = '\0';
	}
	/* operator has been reduced to its smallest 
	 * possible length, now try to find it in the
	 * operator table [tokens.y]
	 */
	for ( ; ; )
	{
		for (op = Optab; op->op_term; op++)
			if (sequal(op->op_term, opbuf))
				break;
		if (!op->op_term && opbuf [1])
		{
			/* reduce a 2 char operator to 1 char,
			 * and re-search
			 */
			backup(opbuf[1]);
			opbuf [1] = '\0';
			continue;
		}
		break;
	}
	if (op->op_term)
	{
		/* string quotes ? */
		if (op->op_token == Tokens.sp_quote)
			return (string(op));

		/* comment indicator ? */
		if (op->op_token == Tokens.sp_bgncmnt)
			return (comment());

		/* {sequal(opbuf, op->op_term)} */
		Opcode = op->op_code;
		yylval.u_dn = addsym(op->op_term);
		return (op->op_token);
	}
	yysemerr("bad operator", opbuf);

	/* operator not found, skip token and try again */
	return (CONTINUE);
}
