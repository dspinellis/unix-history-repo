/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mci.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "sendmail.h"

/*
**  MCI_CACHE -- enter a connection structure into the open connection cache
**
**	This may cause something else to be flushed.
**
**	Parameters:
**		mci -- the connection to cache.
**
**	Returns:
**		none.
*/

mci_cache(mci)
	register MCONINFO *mci;
{
	register MCONINFO **mcislot;
	extern MCONINFO **mci_scan();

	if (MaxMciCache <= 0)
	{
		/* we don't support caching */
		return;
	}

	/*
	**  Find the best slot.  This may cause expired connections
	**  to be closed.
	*/

	mcislot = mci_scan(mci);

	/* if this is already cached, we are done */
	if (bitset(MCIF_CACHED, mci->mci_flags))
		return;

	/* otherwise we may have to clear the slot */
	if (*mcislot != NULL)
		mci_uncache(mcislot);

	*mcislot = mci;
	mci->mci_flags |= MCIF_CACHED;
}
/*
**  MCI_SCAN -- scan the cache, flush junk, and return best slot
**
**	Parameters:
**		savemci -- never flush this one.  Can be null.
**
**	Returns:
**		The LRU (or empty) slot.
*/

MCONINFO	**MciCache;

MCONINFO **
mci_scan(savemci)
	MCONINFO *savemci;
{
	time_t now;
	register MCONINFO **bestmci;
	register MCONINFO *mci;
	register int i;

	if (MciCache == NULL)
	{
		/* first call */
		MciCache = (MCONINFO **) xalloc(MaxMciCache * sizeof *MciCache);
		return (&MciCache[0]);
	}

	now = curtime();
	bestmci = &MciCache[0];
	for (i = 0; i < MaxMciCache; i++)
	{
		mci = MciCache[i];
		if (mci == NULL || mci->mci_state == MCIS_CLOSED)
		{
			bestmci = &MciCache[i];
			continue;
		}
		if (mci->mci_lastuse + MciCacheTimeout < now && mci != savemci)
		{
			/* connection idle too long -- close it */
			bestmci = &MciCache[i];
			mci_uncache(bestmci);
			continue;
		}
		if (*bestmci == NULL)
			continue;
		if (mci->mci_lastuse < (*bestmci)->mci_lastuse)
			bestmci = &MciCache[i];
	}
	return bestmci;
}
/*
**  MCI_UNCACHE -- remove a connection from a slot.
**
**	May close a connection.
**
**	Parameters:
**		mcislot -- the slot to empty.
**
**	Returns:
**		none.
*/

mci_uncache(mcislot)
	register MCONINFO **mcislot;
{
	register MCONINFO *mci;
	extern ENVELOPE *BlankEnvelope;

	mci = *mcislot;
	if (mci == NULL)
		return;
	*mcislot = NULL;
	mci->mci_flags &= ~MCIF_CACHED;

	/* only uses the envelope to flush the transcript file */
	if (mci->mci_state != MCIS_CLOSED)
		smtpquit(mci->mci_mailer, mci, &BlankEnvelope);
}
/*
**  MCI_FLUSH -- flush the entire cache
*/

mci_flush()
{
	register int i;

	if (MciCache == NULL)
		return;

	for (i = 0; i < MaxMciCache; i++)
		mci_uncache(&MciCache[i]);
}
/*
**  MCI_GET -- get information about a particular host
*/

MCONINFO *
mci_get(host, m)
	char *host;
	MAILER *m;
{
	return &(stab(host, ST_MCONINFO + m->m_mno, ST_ENTER))->s_mci;
}
