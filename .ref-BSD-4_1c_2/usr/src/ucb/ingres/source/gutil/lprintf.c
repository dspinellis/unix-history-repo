# include	<useful.h>
# include	<sccs.h>

SCCSID(@(#)lprintf.c	7.1	2/5/81)

/*
**  LPRINTF -- labelled printf
**
**	Just like printf, except outputs the process name first.
**
**	Parameters:
**		fmt -- the format.
**		p1 - p6 -- the parameters.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

lprintf(fmt, p1, p2, p3, p4, p5, p6)
char	*fmt;
{
	extern char	*Proc_name;

	if (Proc_name != NULL)
		printf("%s: ", Proc_name);
	printf(fmt, p1, p2, p3, p4, p5, p6);
}
