/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid [] = "@(#)udb.c	5.1 (Berkeley) %G%";
#endif

#include "sendmail.h"

#ifdef USERDB

#include <sys/file.h>
#include <db.h>

/*
**  UDBEXPAND -- look up user in database and expand
**
**	Parameters:
**		a -- address to expand.
**		sendq -- pointer to head of sendq to put the expansions in.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Modifies sendq.
*/

void
udbexpand(a, sendq)
	register ADDRESS *a;
	ADDRESS **sendq;
{
	int i;
	register char *p;
	auto char *class;
	auto char *list;
	DBT key;
	DBT info;
	static DB *dbp = NULL;
	register char *bp;
	char buf[8192];

	if (tTd(28, 1))
		printf("expand(%s)\n", a->q_paddr);

	/* make certain we are supposed to send to this address */
	if (bitset(QDONTSEND, a->q_flags))
		return;
	CurEnv->e_to = a->q_paddr;

	/* if necessary, open the database */
	if (dbp == NULL)
	{
		if (UdbFileName == NULL || UdbFileName[0] == '\0')
		{
			if (tTd(28, 4))
				printf("no userdb specified\n");
			return;
		}
		dbp = hash_open(UdbFileName, O_RDONLY, 0644, NULL);
		if (dbp == NULL)
		{
			extern int errno;

			if (tTd(28, 2))
				printf("cannot open %s: %d\n", UdbFileName, errno);
			return;
		}
	}

	key.data = a->q_user;
	key.size = strlen(key.data);
	i = dbp->get(dbp, &key, &info, R_NOOVERWRITE);
	if (i != 0 || info.size <= 0)
	{
		if (i < 0)
			syserr("udbexpand: db-get stat %s");
		if (tTd(28, 2))
			printf("expand: no match on %s\n", key.data);
		return;
	}

	/* extract the class (first string) and data (second string) */
	i = strlen((char *) info.data) + 1;
	p = (char *) info.data + i;
	i = info.size - i;

	/* use internal buffer if it will fit; otherwise malloc */
	if (i < sizeof buf)
		bp = buf;
	else
		bp = xalloc(i + 1);
	bcopy(p, bp, i);
	bp[i] = '\0';

	if (tTd(28, 1))
		printf("Class %s: %s\n", info.data, bp);

	/* do special processing based on class */
	if (strcmp((char *) info.data, "user") == 0)
	{
		message(Arpa_Info, "expanded to (%s) %s", info.data, bp);
		AliasLevel++;
		sendtolist(bp, a, sendq);
		AliasLevel--;
	}

	/* free memory if we allocated it */
	if (bp != buf)
		free(bp);
}

#endif /* USERDB */
