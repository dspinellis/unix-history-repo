# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)ingresname.c	7.1	2/5/81)

/*
**  MAKE INGRES FILE NAME
**
**	The null-terminated string 'iname' is converted to a
**	file name as used by the ingres relations.  The name
**	of the relation is padded out to be MAXNAME bytes long,
**	and the two-character id 'id' is appended.  The whole
**	thing will be null-terminated and put into 'outname'.
**
**	'Outname' must be at least MAXNAME + 3 bytes long.
*/

ingresname(iname, id, outname)
char	*iname;
char	*id;
char	*outname;
{
	register char	*p;
	extern char	*pmove();

	p = outname;
	p = pmove(iname, p, MAXNAME, ' ');
	bmove(id, p, 2);
	p[2] = NULL;
}
