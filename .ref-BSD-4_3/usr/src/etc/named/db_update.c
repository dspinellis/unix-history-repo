#ifndef lint
static char sccsid[] = "@(#)db_update.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <syslog.h>
#include <arpa/nameser.h>
#include "db.h"

struct timeval	tt;
extern FILE *ddt;

/*
 * Update data base. Flags control the action.
 * Inverse query tables modified.
 */
db_update(name, odp, newdp, flags)
	char name[];
	struct databuf *odp, *newdp;
	int flags;
{
	register struct namebuf *np;
	register struct databuf *dp, *pdp;
	struct hashbuf *htp = hashtab;
	char *fname;

#ifdef DEBUG
	extern int debug;

	if (debug >= 3)
		fprintf(ddt,"db_update( %s )\n",name);
#endif
	np = nlookup(name, &htp, &fname, newdp != NULL);
	if (np == NULL || fname != name)
		return (NONAME);
	if (odp != NULL) {
		pdp = NULL;
		for (dp = np->n_data; dp != NULL; pdp = dp, dp = dp->d_next) {
			if (!match(dp, odp->d_class, odp->d_type))
				continue;
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"f = %#x, size = %d, %d (%d)\n",
				    flags, odp->d_size, dp->d_size,
				    db_cmp(dp, odp));
#endif
			if ((flags & DB_NODATA) &&
			    !db_cmp(dp, odp)) {
				/* refresh ttl if cache entry */
				if (dp->d_zone == 0) {
					if (dp->d_ttl < tt.tv_sec ) {
						dp->d_ttl = odp->d_ttl;
						return (OK);
					}
					if (dp->d_ttl < odp->d_ttl)
						dp->d_ttl = odp->d_ttl;
				}
				return (DATAEXISTS);
			}
			if ((flags & DB_MEXIST) && db_cmp(dp, odp))
				return (NODATA);
			if (flags & DB_DELETE) {
				if (pdp == NULL)
					np->n_data = dp->d_next;
				else
					pdp->d_next = dp->d_next;
				rminv(dp);
				(void) free((char *)dp);
			}
		}
	}
	if (newdp == NULL)
		return (OK);
	addinv(np, newdp);	/* modify inverse query tables */

	/* Add to end of list, generally preserving order */
	newdp->d_next = NULL;
	if ((dp = np->n_data) == NULL)  {
		np->n_data = newdp;
		return (OK);
	}
	while (dp->d_next != NULL) {
		dp = dp->d_next;
		/* NEEDS: check for duplicate WKS records and flag error */
	}
	dp->d_next = newdp;
	return (OK);
}

struct invbuf *invtab[INVHASHSZ];	/* Inverse query hash table */

/*
 * Add data 'dp' to inverse query tables for name 'np'.
 */
addinv(np, dp)
	struct namebuf *np;
	struct databuf *dp;
{
	register struct invbuf *ip;
	register int hval, i;

	switch (dp->d_type) {
	case T_A:
	case T_UID:
	case T_GID:
		break;

	default:
		return;
	}

	hval = dhash(dp->d_data, dp->d_size);
	for (ip = invtab[hval]; ip != NULL; ip = ip->i_next)
		for (i = 0; i < INVBLKSZ; i++)
			if (ip->i_dname[i] == NULL) {
				ip->i_dname[i] = np;
				return;
			}
	ip = saveinv();
	ip->i_next = invtab[hval];
	invtab[hval] = ip;
	ip->i_dname[0] = np;
}

/*
 * Remove data 'odp' from inverse query table.
 */
rminv(odp)
	struct databuf *odp;
{
	register struct invbuf *ip;
	register struct databuf *dp;
	struct namebuf *np;
	register int i;

	for (ip = invtab[dhash(odp->d_data, odp->d_size)]; ip != NULL;
	    ip = ip->i_next) {
		for (i = 0; i < INVBLKSZ; i++) {
			if ((np = ip->i_dname[i]) == NULL)
				break;
			for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
				if (!match(dp, odp->d_class, odp->d_type))
					continue;
				if (db_cmp(dp, odp))
					continue;
				while (i < INVBLKSZ-1) {
					ip->i_dname[i] = ip->i_dname[i+1];
					i++;
				}
				ip->i_dname[i] = NULL;
				return;
			}
		}
	}
}

/*
 * Compute hash value from data.
 */
dhash(dp, dlen)
	char *dp;
	int dlen;
{
	register char *cp;
	register unsigned hval;
	register int n;

	n = dlen;
	if (n > 8)
		n = 8;
	hval = 0;
	for (cp = dp; --n >= 0; ) {
		hval <<= 1;
		hval += *cp++;
	}
	return (hval % INVHASHSZ);
}

/*
 * Compare data sections from databufs for equivalence.  Must be case
 * insensitive for some domain names.  We assume that they are the
 * same type when they are passed.  Return 0 if equivalent, nonzero
 * otherwise.
 */

db_cmp(dp1, dp2)
	register struct databuf *dp1, *dp2;

{
	register char *cp1, *cp2;
	int len;

	if (dp1->d_size != dp2->d_size)
		return(1);
	switch (dp1->d_type) {

	case T_A:
	case T_UID:
	case T_GID:
	case T_WKS:
	case T_NULL:
		return(bcmp(dp1->d_data, dp2->d_data, dp1->d_size));

	case T_NS:
	case T_CNAME:
	case T_PTR:
	case T_MB:
	case T_MG:
	case T_MR:
	case T_UINFO:
		return(cistrcmp(dp1->d_data, dp2->d_data));

	case T_HINFO:
		cp1 = dp1->d_data;
		cp2 = dp2->d_data;
		len = *cp1;
		if (cistrncmp(++cp1, ++cp2, len))
			return(1);
		cp1 += len;
		cp2 += len;
		len = *cp1;
		return(cistrncmp(++cp1, ++cp2, len));

	case T_SOA:
	case T_MINFO:
		if (cistrcmp(dp1->d_data, dp2->d_data))
			return(1);
		cp1 = dp1->d_data + strlen(dp1->d_data) + 1;
		cp2 = dp2->d_data + strlen(dp2->d_data) + 1;
		if (dp1->d_type != T_SOA)
			return(cistrcmp(cp1, cp2));
		if (cistrcmp(cp1, cp2))
			return(1);
		cp1 += strlen(cp1) + 1;
		cp2 += strlen(cp2) + 1;
		return(bcmp(cp1, cp2, sizeof(u_long) * 5));
	
	case T_MX:
		cp1 = dp1->d_data;
		cp2 = dp2->d_data;
		if (*cp1++ != *cp2++ || *cp1++ != *cp2++)	/* cmp prio */
			return(1);
		return(cistrcmp(cp1, cp2));

	default:
		return (1);
	}
}
