/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
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
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)addr.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

# include "postbox.h"

/*
**  PUTONQ -- put an address node on the end of a queue
**
**	Parameters:
**		a -- the address to put on the queue.
**		q -- the queue to put it on.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Called By:
**		alias
**		recipient
*/

putonq(a, q)
	register ADDRESS *a;
	register ADDRESS *q;
{
	if (q->q_prev == NULL)
	{
		q->q_prev = q->q_next = a;
		a->q_prev = NULL;
	}
	else
	{
		a->q_prev = q->q_prev;
		q->q_prev->q_next = a;
		q->q_prev = a;
	}
	a->q_next = NULL;
}
/*
**  TKOFFQ -- remove address node from queue
**
**	Takes a node off of a queue, from anyplace in the queue.
**
**	Parameters:
**		a -- the node to remove.
**		q -- the queue to remove it from.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Called By:
**		alias
*/

tkoffq(a, q)
	register ADDRESS *a;
	register ADDRESS *q;
{
	if (a->q_prev != NULL)
		a->q_prev->q_next = a->q_next;
	else
		q->q_next = a->q_next;
	if (a->q_next != NULL)
		a->q_next->q_prev = a->q_prev;
	else
		q->q_prev = a->q_prev;
}
/*
**  SAMEADDR -- Determine if tow addresses are the same
**
**	This is not just a straight comparison -- if the mailer doesn't
**	care about the host we just ignore it, etc.
**
**	Parameters:
**		a, b -- pointers to the internal forms to compare.
**		wildflg -- if TRUE, 'a' may have no user specified,
**			in which case it is to match anything.
**
**	Returns:
**		TRUE -- they represent the same mailbox.
**		FALSE -- they don't.
**
**	Side Effects:
**		none.
**
**	Called By:
**		recipient
**		alias
*/

bool
sameaddr(a, b, wildflg)
	register ADDRESS *a;
	register ADDRESS *b;
	bool wildflg;
{
	/* if they don't have the same mailer, forget it */
	if (a->q_mailer != b->q_mailer)
		return (FALSE);

	/* if the user isn't the same, we can drop out */
	if ((!wildflg || a->q_user[0] != '\0') && strcmp(a->q_user, b->q_user) != 0)
		return (FALSE);

	/* if the mailer ignores hosts, we have succeeded! */
	if (bitset(M_NOHOST, Mailer[a->q_mailer]->m_flags))
		return (TRUE);

	/* otherwise compare hosts (but be careful for NULL ptrs) */
	if (a->q_host == NULL || b->q_host == NULL)
		return (FALSE);
	if (strcmp(a->q_host, b->q_host) != 0)
		return (FALSE);

	return (TRUE);
}
