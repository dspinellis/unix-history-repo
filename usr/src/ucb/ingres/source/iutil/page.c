# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)page.c	7.1	2/5/81)

/*
**	UNIX read/write counters
*/

long	Accuread, Accuwrite;
long	Accusread;

/*
**  GETPAGE
**
**	Trace Flags:
**		26
*/

get_page(d, tid)
register DESC	*d;
TID		*tid;
{
	register int		i;
	long			pageid;
	register struct accbuf	*b;
	struct accbuf		*b1;
	int			lk;		/* lock condition*/
	extern struct accbuf	*choose_buf();
	extern long		lseek();

#	ifdef xATR3
	if (tTf(26, 9))
	{
		printf("GET_PAGE: %.14s,", d->reldum.relid);
		dumptid(tid);
	}
#	endif

	pluck_page(tid, &pageid);
	if ((b = choose_buf(d, pageid)) == NULL)
		return (-1);
	top_acc(b);
	lk = Acclock && (d->reldum.relstat & S_CONCUR) && (d->relopn < 0);
	if ((b->thispage != pageid) || (lk && !(b->bufstatus & BUF_LOCKED)))
	{
		if (i = pageflush(b))
			return (i);
#		ifdef xATR1
		if (tTf(26, 10))
		{
			printf("GET_PAGE: rdg pg %ld", pageid);
			printf(",relid ");
			dumptid((TID *) &d->reltid);
		}
#		endif
		b->thispage = pageid;
		if (lk)
		{
			b1 = Acc_head;
			for (; b1 != 0; b1 = b1->modf)
				if (b1->bufstatus & BUF_LOCKED)
					pageflush(b1);  /*  */
			if (setpgl(b) < 0)
				syserr("get-page: lk err");
		}
#		ifdef xATM
		if (tTf(76, 1))
			timtrace(17, &pageid, &d->reltid.ltid);
#		endif
		if ((lseek(d->relfp, (long)(pageid * PGSIZE), 0) == -1) ||
		    (read(d->relfp, (char *) b, PGSIZE) != PGSIZE))
		{
			resetacc(b);
			return (acc_err(AMREAD_ERR));
		}
		Accuread++;
		if (d->reldum.relstat & S_CATALOG)
		{
			Accusread++;
		}
#		ifdef xATM
		if (tTf(76, 1))
			timtrace(18, &pageid, &d->reltid.ltid);
#		endif
	}
	return (0);
}
/*
**  PAGEFLUSH
**
**	Trace Flags:
**		29.2-3
*/

pageflush(buf)
struct accbuf	*buf;
{
	register struct accbuf	*b;
	register int		allbufs;
	int			err;

	b = buf;
#	ifdef xATR3
	if (tTf(29, 2))
	{
		printf("PAGEFLUSH: %x=", b);
		if (b != NULL)
			dumptid(&b->rel_tupid);
		else
			printf("all\n");
	}
#	endif
	err = FALSE;
	allbufs = FALSE;
	if (b == 0)
	{
		b = Acc_buf;
		allbufs = TRUE;
	}

	do
	{
		if (b->bufstatus & BUF_DIRTY)
		{
#			ifdef xATR1
			if (tTf(29, 3))
			{
				printf("PAGEFLUSH: wr pg %ld", b->thispage);
				printf(",stat %d,relid ", b->bufstatus);
				dumptid((TID *) &b->rel_tupid);
			}
#			endif

#			ifdef xATM
			if (tTf(76, 1))
				timtrace(19, &b->thispage, &b->rel_tupid);
#			endif

			b->bufstatus &= ~BUF_DIRTY;
			if ((lseek(b->filedesc, (long)(b->thispage * PGSIZE), 0)== -1) ||
			    (write(b->filedesc, (char *) b, PGSIZE) != PGSIZE))
			{
				resetacc(b);
				err = TRUE;
			}
			Accuwrite++;

#			ifdef xATM
			if (tTf(76, 1))
				timtrace(20, &b->thispage, &b->rel_tupid);
#			endif
		}
		if (Acclock && b->bufstatus & BUF_LOCKED)
			unlpg(b);

	} while (allbufs && (b = b->modf) != NULL);

	if (err)
		return (acc_err(AMWRITE_ERR));

	return (0);
}
/*
**  ACC_ERR -- set global error indicator "Accerror"
**
**	Trace Flags:
**		20.4-5
*/

acc_err(errnum)
int	errnum;
{
	Accerror = errnum;
#	ifdef xATR1
	tTfp(20, 4, "ACC_ERR: %d\n", Accerror);
#	endif
	return (Accerror);
}
