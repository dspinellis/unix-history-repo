# include <ingres.h>
# include "scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_name.c	7.1	2/5/81)

/*
** NAME
** A name is defined to be a sequence of MAXNAME or fewer alphanumeric
** characters, starting with an alphabetic (underscore "_" is considered
** an alphabetic).  If it is not a keyword, each name is entered into
** the symbol table, indexed by 'yylval'.  A token is then returned for
** that name.
*/
name(chr)
char	chr;
{
	extern char		*yylval;
	extern char		Cmap[];
	char			namebuf[MAXNAME + 1];
	register int		hi, lo, curr;
	extern char		*syment();

	/* fill in the name */
	yylval = namebuf;
	*yylval = chr;
	do
	{
		*++yylval = gtchar();
		if ((yylval - namebuf) > MAXNAME)
		{
			/* name too long */
			*yylval = '\0';
			par_error(NAMELONG, WARN, namebuf, 0);
		}

	}  while (Cmap[*yylval] == ALPHA || Cmap[*yylval] == NUMBR);
	backup(*yylval);
	*yylval = '\0';

	/* is it a keyword ? */
	lo = 0;
	hi = Keyent - 1;
	while (lo <= hi)
	{
		curr = (lo + hi) / 2;
		switch (scompare(Keyword[curr].term, MAXNAME, namebuf, MAXNAME))
		{
		  case 1:
			hi = curr - 1;
			continue;

		  case -1:
			lo = curr + 1;
			continue;

		  case 0:
			Lastok.toktyp = Tokens.sconst;
			Lastok.tok = Keyword[curr].term;
			Lastok.tokop = Keyword[curr].opcode;
			yylval = (char *) Lastok.tokop;
			return (Keyword[curr].token);
		}
	}

	/* else, USER DEFINED NAME */
#	ifdef	xSTR2
	tTfp(71, 0, "name: %s\n", namebuf);
#	endif
	yylval = syment(namebuf, length(namebuf) + 1);
	Lastok.tok = yylval;
	Lastok.toktyp = Tokens.sconst;
	Lastok.tokop = 0;
	return (Tokens.name);
}
