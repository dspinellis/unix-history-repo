# include	<sccs.h>

SCCSID(@(#)xfree.c	7.1	2/5/81)

/*
**  XFREE -- free memory only if dynamically allocated.
**
**	This acts just like "free", except that it does nothing
**	if the area handed to it hasn't been dynamically allocated.
**
**	Parameters:
**		p -- a pointer to the area to free.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Free memory queue is changed.
**
**	WARNING:
**		This routine depends on the implementation of malloc
**		in C; it may have to be changed on other systems.
*/

xfree(p)
char	*p;
{
	extern char	end[];

	if (p >= end && p < (char *) &p)
		free(p);
}
