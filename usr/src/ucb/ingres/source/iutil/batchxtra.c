# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<access.h>
# include	<batch.h>
# include	<sccs.h>

SCCSID(@(#)batchxtra.c	7.1	2/5/81)

rmbatch()
{
	char		*batchname();
	register char	*p;
	register int	i;

	p = batchname();
	if (i = close(Batch_fp))
		syserr("rmbatch:can't close %s %d", p, i);
	if (i = unlink(p))
		syserr("rmbatch:can't unlink %s %d", p, i);
	Batchhd.mode_up = 0;
	return (0);
}
/*
**  BATCHNAME -- create batch file name
*/

char *
batchname()
{
	extern char	*Fileset;
	extern char	*ztack();

	return(ztack("_SYSbatch", Fileset));
}
