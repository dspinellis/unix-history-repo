# include <ingres.h>
# include "scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_comment.c	7.1	2/5/81)

/*
** COMMENT
** scans comments (as delimited by the tokens 'Tokens.bgncmnt'
** and 'Tokens.endcmnt') and removes them from the query text.
*/
comment()
{
	register int		i, l;
	register struct optab	*op;
	char			buf[3];

	/* find the end_of_comment operator */
	for (op = Optab; op->term; op++)
		if (op->token == Tokens.endcmnt)
			break;
	if (!op->term)
		syserr("no end_of_comment token");

	/* scan for the end of the comment */
	l = length(op->term);
	for (i = 0; i < l; i++)		/* set up window on input */
		if ((buf[i] = gtchar()) <= 0)
			/* non-terminated comment */
			par_error(COMMTERM, FATAL, 0);		/* must end parsing */
	while (!bequal(buf, op->term, l))
	{
		/* move window on input */
		for (i = 0; i < l-1; i++)
			buf[i] = buf[i+1];
		if ((buf[l-1] = gtchar()) <= 0)
			/* non terminated comment */
			par_error(COMMTERM, FATAL, 0);		/* must end parsing */
	}
	return (0);
}
