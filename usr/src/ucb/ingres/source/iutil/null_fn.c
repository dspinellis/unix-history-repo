# include	<sccs.h>

SCCSID(@(#)null_fn.c	7.1	2/5/81)

/*
**  NULL_FN -- A null function
**
**	This routine does absolutely nothing at all.
**
**	Algorithm:
**		none.
**
**	Parameters:
**		none.
**
**	Returns:
**		zero
**
**	Side Effects:
**		none.
**
**	Defined Constants:
**		none.
**
**	Defines:
**		null_fn
**
**	Requires:
**		nothing.
**
**	Required By:
**		Lots (this system doesn't do much).
**
**	Files:
**		none.
**
**	Compilation Flags:
**		none.
**
**	Trace Flags:
**		none.
**
**	Diagnostics:
**		none.
**
**	Syserrs:
**		none.
**
**	Deficiencies:
**		It should do nothing faster.
**
**	History:
**		5/12/80 (eric & polly) -- written.
**
**	Version:
**		7.1
**
**	WARNING:
**		Do not use this routine if you want to do something.
*/

null_fn()
{
	return (0);
}
