# include	"buf.h"
# include	<sccs.h>

SCCSID(@(#)buf.c	7.1	2/5/81)


/*
**  BUFFER MANIPULATION ROUTINES
*/



/*
**  BUFPUT -- put character onto buffer
**
**	The character 'c' is put onto the buffer 'bp'.  If the buffer
**	need be extended it is.
*/

bufput(c, buffer)
char		c;
struct buf	**buffer;
{
	register struct buf	*b;
	register struct buf	*a;
	register struct buf	**bp;
	char			*bufalloc();

	bp = buffer;
	b = *bp;
	if (b == 0 || b->ptr >= &b->buffer[BUFSIZE])
	{
		/* allocate new buffer segment */
		a = (struct buf *) bufalloc(sizeof *a);
		a->nextb = b;
		a->ptr = a->buffer;
		*bp = b = a;
	}

	*b->ptr++ = c;
}
/*
**  BUFGET -- get character off of buffer
**
**	The buffer is popped and the character is returned.  If the
**	segment is then empty, it is returned to the free list.
*/

bufget(buffer)
struct buf	**buffer;
{
	register struct buf	*b;
	register char		c;
	register struct buf	**bp;

	bp = buffer;
	b = *bp;

	if (b == 0 || b->ptr == b->buffer)
	{
		/* buffer is empty -- return end of file */
		return (0);
	}

	c = *--(b->ptr);

	/* check to see if we have emptied the (non-initial) segment */
	if (b->ptr == b->buffer && b->nextb != 0)
	{
		/* deallocate segment */
		*bp = b->nextb;
		buffree(b);
	}

	return (c);
}
/*
**  BUFPURGE -- return an entire buffer to the free list
**
**	The buffer is emptied and returned to the free list.  This
**	routine should be called when the buffer is to no longer
**	be used.
*/

bufpurge(buffer)
struct buf	**buffer;
{
	register struct buf	**bp;
	register struct buf	*a;
	register struct buf	*b;

	bp = buffer;
	b = *bp;
	*bp = 0;

	/* return the segments to the free list */
	while (b != 0)
	{
		a = b->nextb;
		buffree(b);
		b = a;
	}
}
/*
**  BUFFLUSH -- flush a buffer
**
**	The named buffer is truncated to zero length.  However, the
**	segments of the buffer are not returned to the system.
*/

bufflush(buffer)
struct buf	**buffer;
{
	register struct buf	*b;
	register struct buf	**bp;

	bp = buffer;
	b = *bp;
	if (b == 0)
		return;

	/* return second and subsequent segments to the system */
	bufpurge(&b->nextb);

	/* truncate this buffer to zero length */
	b->ptr = b->buffer;
}
/*
**  BUFCRUNCH -- flatten a series of buffers to a string
**
**	The named buffer is flattenned to a conventional C string,
**	null terminated.  The buffer is deallocated.  The string is
**	allocated "somewhere" off in memory, and a pointer to it
**	is returned.
*/

char	*Buf_flat;

char *
bufcrunch(buffer)
struct buf	**buffer;
{
	register char	*p;
	char		*bufflatten();

	p = bufflatten(*buffer, 1);
	*p = 0;
	*buffer = 0;
	return (Buf_flat);
}

char *
bufflatten(buf, length)
struct buf	*buf;
int		length;
{
	register struct buf	*b;
	register char		*p;
	register char		*q;
	char			*bufalloc();

	b = buf;

	/* see if we have advanced to beginning of buffer */
	if (b != 0)
	{
		/* no, keep moving back */
		p = bufflatten(b->nextb, length + (b->ptr - b->buffer));
	}
	else
	{
		/* yes, allocate the string */
		Buf_flat = p = bufalloc(length);
		return (p);
	}

	/* copy buffer into string */
	for (q = b->buffer; q < b->ptr; )
		*p++ = *q++;

	/* deallocate the segment */
	buffree(b);

	/* process next segment */
	return (p);
}
/*
**  BUFALLOC -- allocate clear memory
**
**	This is similar to the system malloc routine except that
**	it has no error return, and memory is guaranteed to be clear
**	when you return.
**
**	It might be nice to rewrite this later to avoid the nasty
**	memory fragmentation that malloc() tends toward.
**
**	The error processing might have to be modified if used anywhere
**	other than INGRES.
*/

char *
bufalloc(size)
int	size;
{
	register char	*p;
	extern int	(*ExitFn)();	/* defined in syserr.c */
	extern char	*malloc();

	p = malloc(size);
	if (p == NULL)
	{
		printf("Out of memory in macro processor\n");
		(*ExitFn)(-1);
	}

	clrmem(p, size);

	return (p);
}
/*
**  BUFFREE -- free memory
*/

buffree(ptr)
char	*ptr;
{
	register char	*p;

	p = ptr;

	if (p == 0)
		syserr("buffree: 0 ptr");

	free(p);
}
