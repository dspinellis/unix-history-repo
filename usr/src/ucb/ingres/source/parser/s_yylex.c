# include	<ingres.h>
# include	"scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_yylex.c	7.1	2/5/81)

struct special	Tokens;			/* special tokens table */
struct optab	Optab[];		/* operator table */
struct optab	Keyword[];		/* keyword table */
struct lastok	Lastok;
int		Opcode;			/* opcode for current token */
int		Lcase;			/* UPPER->lower conversion flag */
int		Pars;			/* flag for call to getcvar or not */
int		Newline;		/* set if last char read was a newline */
int		Cflag;			/* set if line of C-code recognized */
int		Keyent;			/* number of entries in the Keyword table */

char		Sbuf[SBUFSIZ];		/* symbol table buffer */

/*
** YYLEX
** This is the control program for the scanner (lexical analyzer).
** Each call to yylex() returns a token for the next syntactic unit.
** If the object is of type I2CONST, I4CONST, F8CONST, SCONST or NAME, that
** object will also be entered in the symbol table, indexed by 'yylval'.
** If the object is not one of these types, yylval is the opcode field of
** the operator or keyword tables.
** The end-of-file token is zero.
*/
yylex()
{
	register char	chr;
	register int	rtval;
	extern char	Cmap[];

	rtval = -1;
	Lastok.tokop = 0;
	/* GET NEXT TOKEN */
	do
	{
		if((chr = gtchar()) <= 0)
		{
#			ifdef	xSTR2
			tTfp(72, 8, "end-of-file\n");
#			endif
			rtval = 0;
			break;
		}
		switch(Cmap[chr])
		{
		  case ALPHA:
			rtval = name(chr);
			break;

		  case NUMBR:
			rtval = number(chr);
			break;

		  case OPATR:
			if ((rtval = operator(chr)) == 0)
				rtval = -1;
			break;

		  case PUNCT:
			continue;

		  case CNTRL:
			/* already converted number ? */
			if (Pars)
				switch (chr)
				{
				  case CVAR_I2:
					rtval = getcvar(Tokens.i2const, 2);
					break;

				  case CVAR_I4:
					rtval = getcvar(Tokens.i4const, 4);
					break;

				  case CVAR_F8:
					rtval = getcvar(Tokens.f8const, 8);
					break;

				  case CVAR_S:
					rtval = getcvar(Tokens.sconst, 0);
					break;

				  default:
					printf("funny character 0%o ingnored\n", chr);
					continue;
				}
			break;
		  default:
			syserr("invalid type in yylex()");
		}
	}  while (rtval == -1);
	if (rtval == 0)
	{
		Lastok.tokop = GOVAL;
		Lastok.tok = 0;
		Lastok.toktyp = 0;
	}
	return (rtval);
}


getcvar(type, len)
int	type;
int	len;
{
	extern char	*yylval;
	extern char	Cmap[];
	extern char	*syment();
	register int	save;
	char		buf[MAXSTRING + 1];

	save = Lcase;
	Lcase = 0;
	yylval = buf;
	if (len)
		while ((yylval - buf) < len)
			*yylval++ = gtchar();
	else
	{
		do
		{
			*yylval = gtchar();
			if ((yylval - buf) > MAXSTRING)
			{
				Lcase = save;
				par_error(STRLONG, WARN, 0);
			}
			if (Cmap[*yylval] == CNTRL && *yylval != '\0')
			{
				Lcase = save;
				/* control char in string from equel */
				par_error(CNTRLCHR, WARN, 0);
			}
		} while (*yylval++);
		len = yylval - buf;
	}
	Lcase = save;
	yylval = syment(buf, len);
	Lastok.tok = yylval;
	Lastok.toktyp = type;
	return (type);
}
