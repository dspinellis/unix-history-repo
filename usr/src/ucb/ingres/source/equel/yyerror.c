# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	"y.tab.h"
# include	<sccs.h>

SCCSID(@(#)yyerror.c	7.2	4/7/82)
extern	int	Exit_val;		/* Value to exit with */


/*
**  YYERROR -- Yacc error reporting routine.
**	Yyerror reports on syntax errors encountered by 
**	the yacc parser. Also increments Exit_val.
**
**	Parameters:
**		s -- a string explaining the error
**
**	Returns:
**		none
*/


yyerror(s)
char	*s;
{

	file_spec();
	if (yychar == 0)
		printf("EOF = ");

	if (yylval.u_dn)
		printf("'%s' : ", yylval.u_dn->d_elm);

	printf("line %d, %s\n", yyline, s);
	Exit_val++;
}
/*
**  YYSEMERR -- scanner error reporter
**		Also increments Error_val.
**
**	Parameters:
**		s -- string explaining the error
**		i -- if !0 a string which caused the error
**
**	Returns:
**		none
**
**	Called By:
**		lexical analysis routines -- if called from somewhere else,
**			the line number is likely to be wrong.
*/


yysemerr(s, i)
char		*s;
char		*i;
{
	char	*str;

	file_spec();
	if (i)
		printf("'%s' : ", i);
	printf("line %d, %s\n", yyline, s);
	Exit_val++;
}
/*
**  YYSERROR -- Semantic error reportin routine
**	reports on an error on an entry in the symbol space,
**	using the line number built into the entry. Increments
**	Exit_val.
**
**	Parameters:
**		s -- a string explaining the error
**		d -- a symbol space node
**
**	Returns:
**		none
**
**	Called By:
**		semantic productions
*/


yyserror(s, d)
char			*s;
struct disp_node	*d;
{
	file_spec();
	printf("'%s' : line %d, %s\n",
	d->d_elm, d->d_line, s);
	Exit_val++;
}
/*
**  FILE_SPEC -- If in an included file, specify the name of that file.
*/


file_spec()
{
	if (Inc_files)
		printf("** %s : ", Input_file_name);
}
