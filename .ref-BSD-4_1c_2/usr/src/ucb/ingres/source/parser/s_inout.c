# include <ingres.h>
# include "scanner.h"
# include	<sccs.h>

SCCSID(@(#)s_inout.c	7.1	2/5/81)

/* TWO CHARACTER STACK FOR 'UNGETC' BACKUP */
char	Pchar[2];
int	Pctr;

/*
** GTCHAR
** If 'Lcase' is set, all upper-case alphabetics are 
** mapped into lower-case.
** The correct return is > 0; a return = 0 indicates end-of-file.
*/
gtchar()
{
	extern int	yyline;
	register char	chr;

	/* USE STACKED CHARACTERS IF POSSIBLE */
	if (Pctr)
		chr = Pchar[--Pctr];
	else
		chr = get_scan(NORMAL);

	/* UPDATE LINE COUNTER */
	if (chr == '\n')
		yyline++;

	return ((Lcase && chr >= 'A' && chr <= 'Z') ? (chr + ('a' - 'A')) : chr);
}


/*
** BACKUP
** saves the character argument in the global stack 'Pchar'
**/
backup(chr)
char	chr;
{
	extern int	yyline;

	if (Pctr == 2)
		syserr("overflow in backup()");
	Pchar[Pctr++] = chr;
	if (chr == '\n')
		yyline--;
}
