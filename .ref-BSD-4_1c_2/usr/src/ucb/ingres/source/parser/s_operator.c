# include <ingres.h>
# include "scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_operator.c	7.1	2/5/81)

/*
** OPERATOR
** an operator is defined as any 1-3 character sequence of
** non-alphanumerics.  It looks up each operator in 'Optab'
** and returns the appropriate token.
*/
operator(chr)
char	chr;
{
	extern int		yylval;
	extern char		Cmap[];
	register struct optab	*op;
	register int		save;
	char			buf[4];

	/* get lookahead characer */
	save = Lcase;
	Lcase = 0;
	buf[0] = chr;
	buf[1] = gtchar();
	buf[2] = gtchar();
	buf[3] = '\0';

	/* is it a floating fraction without leading zero ? */
	if (buf[0] == '.' && Cmap[buf[1]] == NUMBR)
	{
		Lcase = save;
		backup(buf[2]);
		backup(buf[1]);
		return(number(chr));
	}

	/* three character operator ? */
	for (op = &Optab[0]; op->term; op++)
		if (sequal(op->term, buf))
			break;
	if (!op->term)
	{
		/* two character operator ? */
		backup(buf[2]);
		buf[2] = '\0';
		for (op = &Optab[0]; op->term; op++)
			if (sequal(op->term, buf))
				break;
		if (!op->term)
		{
			backup(buf[1]);
			buf[1] = '\0';
			for (op = &Optab[0]; op->term; op++)
				if (sequal(op->term, buf))
					break;
			if (!op->term)
			{
				Lcase = save;
				/* invalid operator */
				par_error (BADOP, WARN, 0);
			}
		}
	}
	Lcase = save;
	if(op->token == Tokens.bgncmnt)
		return(comment());
	if(op->token == Tokens.sconst)
		return (string(op));
	Lastok.tok = op->term;
	Lastok.toktyp = Tokens.sconst;
	yylval = op->opcode;
	return (op->token);
}
