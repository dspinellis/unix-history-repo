#ifndef lint
static char sccsid[] = "@(#)db_reload.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

/*
 * Flush and reload data base.
 */

db_reload()
{
	extern char *bootfile;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"reload()\n");
#endif

	if (hashtab != NULL)
		db_free(hashtab);
	db_inv_free();
	hashtab = NULL;
	ns_init(bootfile);
}

db_free(htp)
	struct hashbuf *htp;
{
	register struct databuf *dp;
	register struct namebuf *np;
	struct namebuf **npp, **nppend;

	npp = htp->h_tab;
	nppend = npp + htp->h_size;
	while (npp < nppend) {
	    for (np = *npp++; np != NULL; free((char *)np), np = np->n_next) {
		if (np->n_hash != NULL)
			db_free(np->n_hash);
		(void) free(np->n_dname);
		if (np->n_data == NULL)
			continue;
		for (dp = np->n_data; dp != NULL; dp = dp->d_next)
			(void) free((char *)dp);
	    }
	}
	(void) free((char *)htp);
}

db_inv_free()
{
	register struct invbuf *ip;
	register int i, j;

	for (i = 0; i < INVHASHSZ; i++)
		for (ip = invtab[i]; ip != NULL; ip = ip->i_next)
			for (j = 0; j < INVBLKSZ; j++)
				ip->i_dname[j] = NULL;
}

