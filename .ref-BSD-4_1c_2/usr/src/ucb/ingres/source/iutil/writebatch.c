# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<access.h>
# include	<batch.h>
# include	<sccs.h>

SCCSID(@(#)writebatch.c	7.1	2/5/81)

/*
**  WRBATCH -- write batch file
*/

wrbatch(cp, count)
char	*cp;
int	count;
{
	register char	*c;
	register int	size, cnt;

	cnt = count;
	c = cp;
#	ifdef xATR1
	if (tTf(25, 8))
		printf("wrbatch:%d (%d)\n", cnt, Batch_cnt);
#	endif

	while (cnt)
	{
		Batch_dirty = TRUE;	/* mark this buffer as dirty */
		if (cnt + Batch_cnt > BATCHSIZE)
			size = BATCHSIZE - Batch_cnt;
		else
			size = cnt;
		bmove(c, &Batchbuf.bbuf[Batch_cnt], size);
		c += size;
		Batch_cnt += size;
		cnt -= size;
		if (Batch_cnt == BATCHSIZE)
			flushbatch();
	}
}
/*
**  FLUSHBATCH -- flush batch file
*/

flushbatch()
{
	register int	i;

	if (Batch_cnt)
	{
#		ifdef xATR1
		if (tTf(25, 9))
			printf("flushing %d\n", Batch_cnt + IDSIZE);
#		endif
		if ((i = write(Batch_fp, &Batchbuf, Batch_cnt + IDSIZE)) != Batch_cnt + IDSIZE)
			syserr("flushbatch:can't write %d", i);
		Batch_cnt = 0;
	}
}
