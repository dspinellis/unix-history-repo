/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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
