# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)acc_addbuf.c	7.1	2/5/81)

/*
**
*/

acc_addbuf(bufs, cnt)
struct accbuf	bufs[];
int		cnt;
{
	register struct accbuf	*b, *end;

	b = bufs;
	end = &b[cnt -1];
	acc_init();

	for ( ; b <= end; b++)
	{
		b->bufstatus = 0;
		resetacc(b);
		Acc_tail->modf = b;
		b->modb = Acc_tail;
		b->modf = NULL;
		Acc_tail = b;
	}
}
