# include	<ingres.h>
# include	<symbol.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)put_tuple.c	7.1	2/5/81)



char	*Acctuple;
char	Accanon[MAXTUP];


/*
**  PUT_TUPLE
**
**	Put the canonical tuple in the position
**	on the current page specified by tid
**
**	Trace Flags:
**		27.3-5
*/

put_tuple(tid, tuple, length)
TID	*tid;
char	*tuple;
int	length;
{
	register char	*tp;
	char		*get_addr();

#	ifdef xATR2
	if (tTf(27, 3))
	{
		printf("put_tuple:len=%d,", length);
		dumptid(tid);
	}
#	endif

	/* get address in buffer */
	tp = get_addr(tid);

	/* move the tuple */
	bmove(tuple, tp, length);

	/* mark page as dirty */
	Acc_head->bufstatus |= BUF_DIRTY;
}
/*
**  CANONICAL
**
**	Make the tuple canonical and return the length
**	of the tuple.
**
**	If the relation is compressed then the tuple in
**	compressed into the global area Accanon.
**
**	As a side effect, the address of the tuple to be
**	inserted is placed in Acctuple.
**
**	returns: length of canonical tuple
**
**	Trace Flags:
**		27.6, 7
*/

canonical(d, tuple)
register DESC	*d;
register char	*tuple;
{
	register int	i;

	if (d->reldum.relspec < 0)
	{
		/* compress tuple */
		i = comp_tup(d, tuple);
		Acctuple = Accanon;
	}
	else
	{
		Acctuple = tuple;
		i = d->reldum.relwid;
	}
#	ifdef xATR3
	if (tTf(27, 6))
		printf("canonical: %d\n", i);
#	endif
	return (i);
}
/*
**  COMP_TUP
**
**	Compress the tuple into Accanon. Compression is
**	done by copying INT and FLOAT as is.
**	For CHAR fields, the tuple is copied until a null
**	byte or until the end of the field. Then trailing
**	blanks are removed and a null byte is inserted at
**	the end if any trailing blanks were present.
*/

comp_tup(d, src)
register DESC	*d;
register char	*src;
{
	register char	*dst;
	char		*save;
	char		*domlen, *domtype;
	int		i, j, len;

	dst = Accanon;

	domlen = &d->relfrml[1];
	domtype = &d->relfrmt[1];

	for (i = 1; i <= d->reldum.relatts; i++)
	{
		len = *domlen++ & I1MASK;
		if (*domtype++ == CHAR)
		{
			save = src;
			for (j = 1; j <= len; j++)
			{
				if ((*dst++ = *src++) == NULL)
				{
					dst--;
					break;
				}
			}

			while (j--)
				if (*--dst != ' ')
					break;

			if (j != len)
				*++dst = NULL;

			dst++;
			src = save + len;
		}
		else
		{
			while (len--)
				*dst++ = *src++;

		}
	}
	return (dst - Accanon);
}
/*
**  SPACE_LEFT
**
**	Determine how much space remains on the page in
**	the current buffer. Included in computation
**	is the room for an additional line number
*/

space_left(bp)
register struct accbuf	*bp;
{
	register int	nextoff;
	register int	i;
	register short	*pp;

	nextoff = bp->nxtlino;
#	ifdef xATR3
	if (nextoff < 0 || nextoff > PGSIZE)
		syserr("space_left: nextoff=%d", nextoff);
#	endif

	/* compute space left on page */
	pp = &bp->linetab[-nextoff];
	i = PGSIZE - *pp - (nextoff + 2) * 2;

	/* see if there is also a free line number */
	if (nextoff < MAXLINENO)
		return (i);
	while (++pp <= &bp->linetab[0])
		if (*pp == 0)
			return (i);
	return (0);
}
