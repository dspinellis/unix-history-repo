# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)expr.c	7.1	2/5/81)



/*
**  EXPR -- evaluate expression
**
**	This module evaluates an expression in somewhat standard
**	infix notation.  Several restrictions apply.  There are
**	no variables, since this can be simulated with the macro
**	processor.  No numeric overflow is checked.  There may be
**	no spaces, tabs, or newlines in the expression.
**
**	The text of the expression is read from 'macgetch', so
**	that must be initialized before calling this routine.
**
**	Operators accepted are + - * / < > >= <= = != % ( )
**	& |.
**	Operands may be signed integers.
**	Standard precedence applies.
**
**	An expression can be viewed as a sequence of operands,
**	and operators.  If the terminator is considered to be
**	an operator, then the sequence must be composed
** 	of n matched pairs of operators and operands.  NOT and
**	Negation are considered to be part of the operand and
**	are treated as such.  Thus to evaluate an expression,
**	n pairs are read until the terminator is found as the
**	last operator.
**
**	Parameters:
**		none
**
**	Returns:
**		value of the expression.  Undetermined value
**		on error.
**
**	Side Effects:
**		Macro processing can occur.
**
**	Trace Flags:
**		none
*/



# undef		STACKSIZE
# define 	STACKSIZE       50
# define	RIGHTP		21
# define	END		22
# define	SEPERATOR	0
# define	OR		1
# define	AND		2
# define	EQUALS		3
# define	NEQ		4
# define	LESS		5
# define	LEQ		6
# define	GREATER		7
# define	GEQ		8
# define	ADD		9
# define 	SUBTRACT	10
# define	MULTIPLY	11
# define	DIVIDE		12
# define	MOD		13


int 	ExprPrec[] =			/* Precedence table */
{
	0,	/* filler */
	1,	/* 1 -- OR */
	2,	/* 2 -- AND */
	3,	/* 3 -- EQUALS */
	3,	/* 4 -- NEQ */
	4,	/* 5 -- LESS */
	4,	/* 6 -- LEQ */
	4,	/* 7 -- GREATER */
	4,	/* 8 -- GEQ */
	5,	/* 9 -- ADD */
	5,	/* 10 -- SUBTRACT */
	6,	/* 11 -- MULTIPLY */
	6,	/* 12 -- DIVIDE */
	6	/* 13 -- MOD */
};


int	ExprNstack[STACKSIZE];
int	*ExprNptr;
int	ExprOstack[STACKSIZE];
int	*ExprOptr;
int	ExprError;
char	ExprPeek;











expr()
{
	ExprNptr = ExprNstack;
	ExprOptr = ExprOstack;
	ExprError = FALSE;
	ExprPeek = -1;
	return(valof(END));
}
/*
**  VALOF -- compute value of expression
**
**	This is the real expression processor.  It handles sequencing
**	and precedence.
**
**	Parameters:
**		terminator -- the symbol which should terminate
**			the expression.
**
**	Returns:
**		The value of the expression.
**
**	Side Effects:
**		Gobbles input.
**
**	Requires:
**		exprfind -- to read operands.
**		opfind -- to read operators.
**		exp_op -- to perform operations.
**
**	Called By:
**		expr
**
**	Diagnostics:
**		Extra Parenthesis found: assumed typo
**			An unmatched right parenthesis was read.
**			It was thrown away.
**		Insufficient parenthesis found: assumed zero.
**			An unmatched left parenthesis was left
**			in the operator stack at the end of the
**			expression.  The value zero was taken
**			for the expression.
**
**	Syserrs:
**		none
*/

valof(terminator)
int	terminator;
{
	register int	number;
	register int	operator;

	pushop(SEPERATOR);		/* initialize the stack */

	for(;;)
	{
		number = exprfind();
		if (ExprError) 
			return(0);
		operator = opfind();
		if (ExprError)
			return(0);

		if (operator == RIGHTP || operator == END)
			break;

		/* Do all previous operations with a higher precedence */
		while (ExprPrec[operator] <= ExprPrec[ExprOptr[-1]])	
			number = exp_op(popop(), popnum(), number);
		pushop(operator);
		pushnum(number);
	}
	if (operator != terminator)		/* ExprError in operators */
		if (operator == RIGHTP)
			printf("Extra parenthesis found: assumed typo.\n");
		else
		{
			ExprError = TRUE;
			printf("Insufficient parenthesis found: Assumed zero.\n");
			return(0);
		}
	/* Empty stack for this call of valof */
	while ((operator = popop()) != SEPERATOR)
		number = exp_op(operator, popnum(), number);

	return(number);
}
/*
**  EXPRFIND -- find and chomp operand
**
**	This routine reads the next operand.  It generally just
**	reads numbers, except it also knows about unary operators
**	! and - (where it calls itself recursively), and paren-
**	theses (where it calls valof recursively).
**
**	Parameters:
**		none
**
**	Returns:
**		value of operand.
**
**	Side Effects:
**		Gobbles input.
**
**	Requires:
**		numberget -- to read numbers.
**		exprgch.
**
**	Called By:
**		valof
**		exprfind (recursively)
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		Expression expected: end of expression found.
**			Nothing was found.  Zero is returned.
**		Expression expected: %c found; assumed zero.
**			A syntax error -- nothing was found
**			which was acceptable.
*/



exprfind()
{
	register int	result;
	register int	c;

	c = exprgch();

	switch(c)
	{
 
	  case '0':
	  case '1':
	  case '2':
	  case '3':
	  case '4':
	  case '5':
	  case '6':
	  case '7':
	  case '8':
	  case '9':
		return(numberget(c));

	  case '!':
		result = exprfind();
		return(ExprError ? 0 : (result <= 0));

	  case '-':
		result = exprfind();
		return(ExprError ? 0 : -result);

	  case '(':
		return(valof(RIGHTP));

	  case ' ':
	  case '\n':
	  case '/t':
	  case '\0':
		printf("Expression expected: end of expression found.\n");
		ExprError = TRUE;
		return(0);

	  default:
		printf("Expression expected; '%c' found: Assumed zero.\n", c);
		ExprError = TRUE;
		return(0);
	}
}
/*
**  OPFIND -- find and translate operator
**
**	This reads the next operator from the input stream and
**	returns the internal code for it.
**
**	Parameters:
**		none
**
**	Returns:
**		The code for the next operator.
**		Zero on error.
**
**	Side Effects:
**		Gobbles input.
**
**	Requires:
**		exprgch.
**
**	Called By:
**		valof
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		Operator expected: '%c' found.
**			Gibberish in input.
*/

opfind()
{
	register int	c;

	c = exprgch();

	switch(c)
	{
	  
	  case '/':
		return(DIVIDE);

	  case '=':
		return(EQUALS);

	  case  '&':
		return(AND);

	  case '|':
		return(OR);

	  case '+':
		return(ADD);

	  case '-':
		return(SUBTRACT);

	  case '*':
		return(MULTIPLY);

	  case '<':
		c = exprgch();
		if (c == '=')
		{
			return(LEQ);
		}
		ExprPeek = c;
		return(LESS);

	  case '>':
		c = exprgch();
		if (c == '=')
		{
			return(GEQ);
		}
		ExprPeek = c;
		return(GREATER);

	  case '%':
		return(MOD);

	  case '!':
		c = exprgch();
		if (c == '=')
		{
			return(NEQ);
		}
		else
		{
			printf("Operator expected: '!%c' found.\n", c);
			ExprError = TRUE;
			return(0);
		}

	  case ')':
		return(RIGHTP);

	  case ' ':
  	  case '\t':
	  case '\n':
	  case '\0':
		return(END);

	  default:
		printf("Operator expected: '%c' found.\n", c);
		ExprError = TRUE;
		return(0);
		
	}
}
/*
**  EXP_OP -- perform operation
**
**	Performs an operation between two values.
**
**	Parameters:
**		op -- the operation to perform.
**		lv -- the left operand.
**		rv -- the right operand.
**
**	Returns:
**		The value of the operation.
**
**	Side Effects:
**		none
**
**	Requires:
**		none
**
**	Called By:
**		valof.
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		none
*/

exp_op(op, lv, rv)
int	op;
int	lv;
int	rv;
{
	switch(op)
	{

	  case OR:
		return((lv > 0) || (rv > 0));

	  case AND:
		return((lv > 0) && (rv > 0));

	  case EQUALS:
		return(lv == rv);

	  case NEQ:
		return(lv != rv);

	  case LESS:
		return(lv < rv);

	  case LEQ:
		return(lv <= rv);

	  case GREATER:
		return(lv > rv);

	  case GEQ:
		return(lv >= rv);

	  case ADD:
		return(lv + rv);

	  case SUBTRACT:
		return(lv - rv);

	  case MULTIPLY:
		return(lv * rv);

	  case DIVIDE:
		if (rv == 0) 
		{
			printf("Divide by zero: zero assumed.\n");
			return(0);
		}
		else
			return(lv / rv);

	  case MOD:
		return(lv % rv);

	  default:
		syserr("exp_op: bad op %d", op);

	}
}
/*
**  NUMBERGET -- read and convert a number
**
**	Reads and converts a signed integer.
**
**	Parameters:
**		none
**
**	Returns:
**		The next number in the input stream.
**
**	Side Effects:
**		Gobbles input.
**
**	Requires:
**		exprgch.
**
**	Called By:
**		exprfind.
*/

numberget(cx)
char	cx;
{
	register int	result;
	register int	c;

	c = cx;

	result = 0;
	do
	{
		result = result * 10 + c - '0';
		c = exprgch();
	} while (c >= '0' && c <= '9');
	ExprPeek = c;
	return(result);
}
/*
**  EXPRGCH -- expression character get
**
**	Gets the next character from the expression input.  Takes
**	a character out of ExprPeek first.  Also maps spaces, tabs,
**	and newlines into zero bytes.
**
**	Parameters:
**		none
**
**	Returns:
**		Next character.
**
**	Side Effects:
**		Gobbles input.
**		Clears ExprPeek if set.
**
**	Requires:
**		ExprPeek -- the peek character.
**		macgetch -- to get the next character if ExprPeek
**			is not set.
*/

exprgch()
{
	register int	c;

	c = ExprPeek;
	if (c < 0)
		c = macgetch();
	ExprPeek = -1;
	if (c == ' ' || c == '\n' || c == '\t')
		c = 0;
	return (c);
}
/*
**  Stack operations.
*/


/* Popop returns the top of the operator stack and decrements this stack. */
popop()
{
	if (ExprOptr <= ExprOstack)
		syserr("popop: underflow");
	return(*--ExprOptr);
}



/* Pushop increments the stack pointer and pushes op on the stack. */
pushop(op)
int	op;
{
	*ExprOptr++ = op;
}



/* Popnum returns the top of the number stack and decrements the stack pointer. */
popnum()
{
	if (ExprNptr <= ExprNstack)
		syserr("popnum: underflow");
	return(*--ExprNptr);
}




/* Pushnum increments the stack pointer and pushes num onto the stack */
pushnum(num)
int 	num;
{
	*ExprNptr++ = num;
}
