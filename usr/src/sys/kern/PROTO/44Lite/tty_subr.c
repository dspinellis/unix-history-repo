/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)tty_subr.c	7.7 (Berkeley) 5/9/91
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "clist.h"

/*
 * Initialize clists.
 */
cinit()
{

	/*
	 * Body deleted.
	 */
}

/*
 * Get a character from a clist.
 */
getc(clp)
	struct clist *clp;
{
	char c;

	/*
	 * Body deleted.
	 */
	return (c);
}

/*
 * Copy clist to buffer.
 * Return number of bytes moved.
 */
q_to_b(clp, cp, count)
	struct clist *clp;
	char *cp;
	int count;
{
	int s, moved = 0;

	if (count <= 0)
		return (0);
	s = spltty();
	/*
	 * Body deleted.
	 */
	splx(s);
	return (moved);
}

/*
 * Return count of contiguous characters in clist.
 * Stop counting if flag&character is non-null.
 */
ndqb(clp, flag)
	struct clist *clp;
	int flag;
{
	int count = 0;
	int s;

	s = spltty();
	/*
	 * Body deleted.
	 */
	splx(s);
	return (count);
}

/*
 * Flush count bytes from clist.
 */
ndflush(clp, count)
	struct clist *clp;
	int count;
{
	int s;

	s = spltty();
	/*
	 * Body deleted.
	 */
	splx(s);
}

/*
 * Put a character into the output queue.
 */
putc(c, clp)
	char c;
	struct clist *clp;
{
	int s, error = 0;

	s = spltty();
	/*
	 * Body deleted.
	 */
	if (error) {
		splx(s);
		return (-1);
	}
	splx(s);
	return (0);
}

/*
 * Copy buffer to clist.
 * Return number of bytes not transfered.
 */
b_to_q(cp, count, clp)
	char *cp;
	int count;
	struct clist *clp;
{
	int s, resid;

	if (count <= 0)
		return (0);
	resid = count;
	s = spltty();
	/*
	 * Body deleted.
	 */
	splx(s);
	return (resid);
}

/*
 * Given a non-NULL pointer into the clist return the pointer
 * to the next character in the list or return NULL if no more chars.
 *
 * Callers must not allow getc's to happen between nextc's so that the
 * pointer becomes invalid.  Note that interrupts are NOT masked.
 */
char *
nextc(clp, cp, count)
	struct clist *clp;
	char *cp;
	int *count;
{
	int empty = 0;

	/*
	 * Body deleted.
	 */
	if (!empty)
		return (cp);
	return (0);
}

/*
 * Remove the last character in the clist and return it.
 */
unputc(clp)
	struct clist *clp;
{
	char c;
	int s;

	s = spltty();
	/*
	 * Body deleted.
	 */
	splx(s);
	return (c);
}

/*
 * Put the chars in the from queue on the end of the to queue.
 */
catq(from, to)
	struct clist *from, *to;
{
	char c;

	while ((c = getc(from)) >= 0)
		putc(c, to);
}
