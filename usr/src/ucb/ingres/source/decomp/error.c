# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)error.c	7.1	2/5/81)


derror(eno)
{
	endovqp(NOACK);
	reinit();
	error(eno, 0);
}
