# include <ingres.h>
# include "scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_string.c	7.1	2/5/81)

/*
** STRING
** A string is defined as any sequence of MAXSTRING or fewer characters,
** surrounded by string delimiters.  New-line ;characters are purged
** from strings unless preceeded by a '\'; QUOTE's must be similarly
** prefixed in order to be correctly inserted within a string.  Each
** string is entered in the symbol table, indexed by 'yylval'.  A
** token or the error condition -1 is returned.
*/
string(op)
struct optab	*op;
{
	extern char	Cmap[];
	extern char	*yylval;
	extern char	*syment();
	register int	esc;
	register int	save;
	register char	*ptr;
	char		buf[MAXSTRING + 1];

	/* disable case conversion and fill in string */
	ptr = buf;
	save = Lcase;
	Lcase = 0;
	do
	{
		/* get next character */
		if ((*ptr = gtchar()) <= 0)
		{
			Lcase = save;
			/* non term string */
			par_error(STRTERM, FATAL, 0);
		}

		/* handle escape characters */
		esc = (*ptr == '\\');
		if (*ptr == '\n')
		{
			if ((*ptr = gtchar()) <= 0)
			{
				Lcase = save;
				*ptr = 0;
				/* non term string */
				par_error(STRTERM, FATAL, 0);
			}
		}
		if (esc == 1)
		{
			if ((*++ptr = gtchar()) <= 0)
			{
				Lcase = save;
				*ptr = 0;
				/* non term string */
				par_error(STRTERM, FATAL, 0);
			}
			if (*ptr == *(op->term))
				*--ptr = *(op->term);
		}

		/* check length */
		if ((ptr - buf) > MAXSTRING - 1)
		{
			Lcase = save;
			/* string too long */
			par_error(STRLONG, WARN, 0);
		}
		if (Cmap[*ptr] == CNTRL)
			/* cntrl in string from equel */
			par_error(CNTRLCHR, WARN, 0);
	} while (*ptr++ != *(op->term) || esc == 1);

	/* restore case conversion and return */
	*--ptr = '\0';
	Lcase = save;
#	ifdef	xSTR2
	tTfp(71, 8, "STRING: %s\n", buf);
#	endif
	yylval = syment(buf, (ptr - buf) + 1);
	Lastok.tok = yylval;
	Lastok.toktyp = Tokens.sconst;
	return (Tokens.sconst);
}
