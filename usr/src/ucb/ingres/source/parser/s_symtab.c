# include	<ingres.h>
# include	<aux.h>
# include	"scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_symtab.c	7.1	2/5/81)

/*
** SYMENT
**	enter a symbol into the symbol table
*/
char *
syment(ptr, len1)
char	*ptr;
int	len1;
{
	register int	len;
	register char	*p;
	extern char	*need();

	len = len1;
	p = need(Sbuf, len);
	bmove(ptr, p, len);
	return (p);
}

/*
** FREESYM
**	free all entries in the symbol table
*/
freesym()
{
	extern int	neederr();

	initbuf(Sbuf, SBUFSIZ, SBUFOFLO, neederr);
}
