# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)printdesc.c	7.1	2/5/81)

/*
**  PRINT RELATION DESCRIPTOR (for debugging)
**
**	A pointer of a file descriptor is passed.  All pertinent
**	info in that descriptor is printed on the standard output.
**
**	For debugging purposes only
*/

printdesc(d)
register DESC	*d;
{
	register int	i;
	register int	end;

	printf("Descriptor @ %x %.12s %.2s (%.12s)\n", d,
	    d->reldum.relid, d->reldum.relowner, d->relvname);
	printf("spec %d, indxd %d, stat %d, save %s",
		d->reldum.relspec, d->reldum.relindxd, d->reldum.relstat,
		ctime(&d->reldum.relsave));
	printf("tups %ld, atts %d, wid %d, prim %ld, stamp %s",
		d->reldum.reltups, d->reldum.relatts, d->reldum.relwid,
		d->reldum.relprim, ctime(&d->reldum.relstamp));
	printf("fp %d, opn %d, adds %ld, ",
		d->relfp, d->relopn, d->reladds);
	dumptid(&d->reltid);

	end = d->reldum.relatts;
	for (i = 0; i <= end; i++)
	{
		printf("[%2d] off %3d fmt %c%3d, xtra %3d, given %3d\n",
			i, d->reloff[i], d->relfrmt[i],
			d->relfrml[i] & 0377, d->relxtra[i], d->relgiven[i]);
	}
}
