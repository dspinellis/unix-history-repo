# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<lock.h>
# include	<opsys.h>
# include	<sccs.h>

SCCSID(@(#)accbuf.c	7.1	2/5/81)


/*
**	access method buffers and other data areas for buffer maintenance
*/

struct accbuf	Acc_buf[NACCBUFS];	/* the buffers */
struct accbuf	*Acc_head;		/* head of usage list */
struct accbuf	*Acc_tail;		/* tail of usage list */
struct lockreq	Lock;

/*
**	structs for admin file data
*/

struct admin	Admin;

/*
**	global flag indicating if access methods
**	have been initialized.
*/

int		Acc_init	=	FALSE;

char		Acclock;		/* locks enabled flag */
int		Alockdes;		/* file descriptor for lock device*/
int		Lockrel;		/* lock relations flag*/
/*
**	Flush the indicated page and reset all
**	important information including the name
**
**	Trace Flags:
**		20.0,1
*/

resetacc(buf)
struct accbuf	*buf;
{
	register struct accbuf	*b;
	register int		i;

	b = buf;
	if (b == 0)
		b = Acc_head;
#	ifdef xATR3
	if (tTf(20, 0))
	{
		printf("RESETACC: %x=", b);
		dumptid((TID *) &b->rel_tupid);
	}
#	endif

	i = pageflush(b);	/* write the page if necessary */
	b->rel_tupid = -1;
	b->filedesc = -1;
	b->thispage = -1;
	b->bufstatus = 0;
	return (i);
}
/*
**	initialize access method data areas
**
**	Trace Flags:
**		20.2,3
*/

acc_init()
{
	register struct accbuf	*last;
	register struct accbuf	*b;
	struct stat		stbuf;
	extern int		errno;

#	ifdef xATR3
	if (tTf(20, 2))
		printf("ACC_INIT=%d\n", Acc_init);
#	endif

	if (Acc_init)
		return;		/* already initialized */
	last = 0;
	for (b = Acc_buf; b < &Acc_buf[NACCBUFS]; )
	{
		resetacc(b);
		b->modb = last;
		last = b;
		b++;
		last->modf = b;
	}
	last->modf = 0;
	Acc_head = Acc_buf;
	Acc_tail = last;

	/* get the admin file */
	readadmin();

	/*
	** Set up locking information. If the database has concurrency
	** control then Lockrel = TRUE and the concurrency device will
	** be opened for writing. If there is no concurrency for the
	** data base or if the lock device isn't installed, then Alockdes
	** = -1 and no locking will (or can) occure.
	*/
	Lockrel = (Admin.adhdr.adflags & A_DBCONCUR) != 0;
	if (Lockrel && Alockdes < 0)
		Alockdes = open("/dev/lock", 1);
	errno = 0;	/* clear in case /dev/lock isn't available */
	Acclock = TRUE;
	stat(".", &stbuf);
	bmove((char *) &stbuf, (char *) Lock.dbnode, 4);

	Acc_init = TRUE;
}
/*
**	place buffer at top of LRU list
*/

top_acc(buf)
struct accbuf	*buf;
{
	register struct accbuf	*b;

	b = buf;

	if (b == Acc_head)
		return (0);
	if (b == Acc_tail)
		Acc_tail = b->modb;
	else
		b->modf->modb = b->modb;
	b->modb->modf = b->modf;
	Acc_head->modb = b;
	b->modf = Acc_head;
	Acc_head = b;
	b->modb = 0;
	return (0);
}
/*
** Flush_rel -- flush all pages associated with the relation
**	described by the descriptor. If resetflag is TRUE,
**	then the buffers are reset so the pages will not be
**	found on subsequent calls to find_page().
**
**	Returns "or'ed" result from calls to pageflush.
**
**	Trace Flags:
**		20.4-5
*/

flush_rel(d, resetflag)
register DESC	*d;
int		resetflag;
{
	register struct accbuf	*b;
	register int		i;

#	ifdef xATR3
	if (tTf(20, 4))
		printf("flush_rel: rel=%.14s, reset=%d\n", d->reldum.relid, resetflag);
#	endif

	i = 0;
	for (b = Acc_head; b != NULL; b = b->modf)
	{
		if (d->reltid.ltid == b->rel_tupid)
		{
			if (resetflag)
				i |= resetacc(b);
			else
				i |= pageflush(b);
		}
	}

	return (i);
}
/*
**	CHOOSE_BUF -- Try to find an empty buffer for assignment.
**		If there is no empty buffer, pick the last buffer
**		in the LRU queue and make sure it is flushed.
**
**		Choose_buf guarantees that the buffer will be reset
**		if it was used previously for a different relation.
**
**	Choose_buf -- choose a buffer for use with the given relation on
**	the given page. The current algorithm is to allow only one buffer
**	per relation. If a relation does not have a buffer, it is given a
**	free one (if any) or else the Least Recently Used.
**
**	Trace Flags:
**		29.0,1
*/

struct accbuf *
choose_buf(dx, pageid)
DESC	*dx;
long	pageid;
{
	register struct accbuf	*b, *free;
	register DESC		*d;
	struct accbuf		*mine;

	d = dx;
	free = mine = NULL;

	for (b = Acc_head; b != 0; b = b->modf)
	{
		if (b->rel_tupid == -1)
			free = b;
		else
			if (d->reltid.ltid == b->rel_tupid)
			{
				if (pageid == b->thispage)
				{
					if (d->relopn < 0)
						b->filedesc = d->relfp;
					return (b);
				}
				mine = b;
			}
	}

	/*
	** "Free" and "Mine" now reflect the current state of the buffers.
	** There is no buffer with the currently requested page
	*/

#	ifdef xATR3
	if (tTf(29, 1))
		printf("choosebuf free %x,mine %x\n", free, mine);
#	endif

	/* no current buffer. Choose a free one or LRU */
	if (free == NULL)
		free = resetacc(Acc_tail) ? NULL : Acc_tail;	/* error if can't reset the LRU */
	if (free)
	{
		/* copy relevant material (in this order in case of rubout) */
		free->filedesc = d->relfp;
		free->rel_tupid = d->reltid.ltid;
	}

#	ifdef xATR1
	if (tTf(29, 0))
		printf("choosebuf:rets %x\n", free);
#	endif
	return (free);
}
/*
**	ACC_CLOSE -- flush any buffers left around
**		and then close the files for relation & attribute.
**		The relation and attribute relation are normally left open
**		until the end of an INGRES session but must be closed
**		and re-opened in the dbu's whenever a new overlay is loaded.
*/

acc_close()
{
	register int	i;

	if (i = pageflush((struct accbuf *) NULL))
		syserr("acc_close: pageflush %d", i);
	close(Admin.adreld.relfp);
	close(Admin.adattd.relfp);
	Admin.adreld.relopn = Admin.adattd.relopn = 0;
	if (Alockdes >= 0)
		close(Alockdes);
	Alockdes = -1;
	Acc_init = FALSE;
	return (0);
}
