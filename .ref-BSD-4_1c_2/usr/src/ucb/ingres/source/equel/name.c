# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	"y.tab.h"
# include	<sccs.h>

SCCSID(@(#)name.c	7.1	2/5/81)


/*
**  NAME -- Process an identifier or keyword token.
**
**	Name gets the identifier that follows in the std.
**	input, and checks if it is a keyword.
**	An identifier is defined as a sequence of
**	MAXNAME or fewer alphanumerics, starting with an
**	alphabetic character.
**
**	Parameters:
**		chr - the first character of the identifier
**
**	Returns:
**		Tokens.sp_name - for a user-defined name
**		Tokens.sp_struct_var -- if the name is declared 
**			a structurw variable
**		other - lexical codes for keys
**
**	Side Effects:
**		Adds a token to the symbol space.
**		yylval is set to the new node in the space.
**		If the identifier is a keyword, sets Opcode to
**		op_code from tokens.y.
*/

name(chr)
char		chr;
{
	int			lval;
	register		i;
	char			wbuf [MAXNAME + 1];
	register char		*cp;
	register char		c;
	struct optab		*op;
	struct optab		*getkey();
	struct cvar		*getcvar();

	c = chr;
	cp = wbuf;
	for (i = 0; i <= MAXNAME; i++)
	{
		lval = Cmap [c];
		if (i < MAXNAME &&
		   (lval == ALPHA || lval == NUMBR))
		{
			*cp++ = c;
			c = getch();
		}
		else if (lval == ALPHA || lval == NUMBR)
		{
			/* {i == MAXNAME && "c is legal" && 
			 *  cp == &wbuf [MAXNAME]} 
			 */
			*cp = '\0';
			yysemerr("name too long", wbuf);
			/* chomp to end of identifier */

			do
			{
				c = getch();
				lval = Cmap [c];
			}  while (lval == ALPHA || lval == NUMBR);
			backup(c);
			
			/* take first MAXNAME characters as IDENTIFIER 
			 * (non-key)
			 */
			yylval.u_dn = addsym(salloc(wbuf));
			return (Tokens.sp_name);
		}
		else
		{
			/* {cp <= &wbuf [MAXNAME] && i <= MAXNAME
			 * && "c is not part of id"}
			 */
			backup(c);
			*cp = '\0';
			i = 0;
			break;
		}
	}
	op = getkey(wbuf);

	/* Is it a keyword ? */
	if (op)
	{
		yylval.u_dn = addsym(op->op_term);
		Opcode = op->op_code;
		return (op->op_token);
	}
	/* user-defined name */
	yylval.u_dn = addsym(salloc(wbuf));
	if (getcvar(wbuf)->c_type == opSTRUCT)
		return(Tokens.sp_struct_var);
	return (Tokens.sp_name);
}
