# include	<ingres.h>
# include	<access.h>
# include 	<opsys.h>
# include	<sccs.h>

SCCSID(@(#)last_page.c	7.1	2/5/81)

/*
**	LAST_PAGE -- computes a tid for the last page in the relation.
*/

# ifdef xV6_UNIX
struct stat
{
	char	junk[9], size0;
	int	size1;
	char	junk2[25];
};
# endif

last_page(d, tid, buf)
register DESC		*d;
register TID		*tid;
register struct accbuf	*buf;
{
	long		lpage;
	struct stat	stats;

	if ((buf != 0) && (abs(d->reldum.relspec) == M_HEAP) && (buf->mainpg == 0) && (buf->ovflopg == 0))
		lpage = buf->thispage;
	else
	{
		if (fstat(d->relfp, &stats))
			syserr("last_page: fstat err %.14s", d->reldum.relid);
#		ifdef xV6_UNIX
		/* number of pages in relation - 1 */
		lpage = ((stats.size1 >> 9) & 0177) + ((stats.size0 & 0377) << 7)- 1;
#		else
		lpage = stats.st_size / PGSIZE - 1;
#		endif
#		ifdef xATR2
		if (tTf(26, 8))
			printf("fstat-lp %.12s %ld\n", d->reldum.relid, lpage);
#		endif
	}
	stuff_page(tid, &lpage);
	tid->line_id = 0;
	return (0);
}
