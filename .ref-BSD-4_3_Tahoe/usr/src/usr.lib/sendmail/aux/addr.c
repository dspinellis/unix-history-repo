/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
static char	SccsId[] = "@(#)addr.c	5.1 (Berkeley) 6/7/85";
#endif not lint

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
