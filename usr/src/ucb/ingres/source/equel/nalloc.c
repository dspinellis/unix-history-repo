# include	<sccs.h>

SCCSID(@(#)nalloc.c	7.1	2/5/81)


/*
**	NALLOC --
**		Dynamic allocation routine which
** 		merely calls alloc(III), returning 
**		0 if no core and a pointer otherwise.
**
*/

char *nalloc(s)
int	s;
{
#	ifdef PDP11
	unsigned	size;

	size = s;
	size = alloc(size);
	if (size == -1)
		return (0);
	return (size);

#	else
	extern char	*malloc();

	return (malloc(s));
#	endif
}
/*
**	SALLOC -- allocate
**		place for string and initialize it,
**		return string or 0 if no core.
**
*/

char *salloc(s)
char		*s;
{
	register unsigned	i;
	register char		*string;
	char			*malloc();

	string = s;
#	ifdef PDP11
	i = alloc(length(string) + 1);
	if (i == -1)
		return (0);
#	else
	i = (unsigned)malloc(length(string) + 1);
#	endif

	if (i)
		smove(string, i);
	return ((char *)i);
}

/*
**	XFREE -- Free possibly dynamic storage
**		checking if its in the heap first.
**
**		0 - freed
**		1 - not in heap
**
*/

xfree(cp)
char		*cp;
{
	extern 			end;	/* break (II) */
	register char		*lcp, *lend, *lacp;

	lcp = cp;
	lacp = (char *)&cp;
	lend = (char *)&end;
	if (lcp >= lend && lcp < lacp)	/* make sure its in heap */
	{
		free(lcp);
		return (0);
	}
	return (1);
}
