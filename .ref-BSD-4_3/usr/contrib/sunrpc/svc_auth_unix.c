/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#ifndef lint
static char sccsid[] = "@(#)svc_auth_unix.c 1.4 85/03/14 Copyr 1984 Sun Micro";
#endif

/*
 * svc_auth_unix.c
 * Handles UNIX flavor authentication parameters on the service side of rpc.
 * There are two svc auth implementations here: AUTH_UNIX and AUTH_SHORT.
 * _svcauth_unix does full blown unix style uid,gid+gids auth,
 * _svcauth_short uses a shorthand auth to index into a cache of longhand auths.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include <stdio.h>
#include "types.h"
#include <sys/time.h>
#include <netinet/in.h>
#include "xdr.h"
#include "auth.h"
#include "clnt.h"
#include "rpc_msg.h"
#include "svc.h"
#include "auth_unix.h"
#include "svc_auth.h"
char *mem_alloc();

#define SHORT_VERF_SIZE (3 * BYTES_PER_XDR_UNIT)
#define CACHE_SIZE 16

static struct cache_entry {
	u_long		sh;		/* short hand value */
#define SHORT_CRED_SIZE (sizeof (u_long))
	short		cred_len;	/* byte length of opaque credential */
	caddr_t		cred_base;	/* the opaque credential body */
	struct authunix_parms aup;	/* cooked, deserialized credentials */
} cache[CACHE_SIZE];

static short cacheindex[CACHE_SIZE];

/*
 * Cache handling macros
 */
#define valid_aup(aup) (TRUE)
#define nexti(i)  ((i == CACHE_SIZE-1) ? 0 : i+1)
#define previ(i)  ((i == 0) ? CACHE_SIZE-1 : i-1)
#define cache_hit(c, d) \
   ( hits++, d=cnt-c, depth += d, maxdepth = (d > maxdepth) ? d: maxdepth )

/*
 * Cache handling routines
 */
static short	find_short_hand();
static short	find_long_hand();

/*
 * Cache variables
 */
static short head, maxdepth;	/* values from 0 to CACHE_SIZE-1, inclusive */
static short cnt;		/*  values from 0 to CACHE_SIZE, inclusive */
static u_long additions, deletions, queries, hits, depth;
static struct timeval last_time;
static inited = 0;		/* stupid kludge to be sure init gets called */


/*
 * Unix longhand authenticator
 */
enum auth_stat
_svcauth_unix(rqst, msg)
	register struct svc_req *rqst;
	register struct rpc_msg *msg;
{
	register short i = -1;
	register int len = msg->rm_call.cb_cred.oa_length;
	register caddr_t base = msg->rm_call.cb_cred.oa_base;
	register enum auth_stat stat = AUTH_OK;
	XDR xdrs;
	struct authunix_parms aup;
	struct opaque_auth short_cred;

	if (!inited) {
		svcauth_unix_init();
	}
	while ((i = find_long_hand(base, len)) < 0) {
		/* deserialize credentials */
		aup.aup_machname = NULL;
		aup.aup_gids = (int *)NULL;
		xdrmem_create(&xdrs, base, (u_int)len, XDR_DECODE); 
		if (! (xdr_authunix_parms(&xdrs, &aup) && valid_aup(&aup))) {
			xdrs.x_op = XDR_FREE;
			(void)xdr_authunix_parms(&xdrs, &aup);
			stat = AUTH_BADCRED;
			goto done;
		}
		/* now make a new cache entry for this credential */
		cache_new_user(base, len, &aup);
	}
	rqst->rq_clntcred = (caddr_t)&(cache[i].aup);
	/* now build a verifier that suggests using the short hand credential */
	short_cred.oa_flavor = AUTH_SHORT;
	short_cred.oa_length = SHORT_CRED_SIZE;
	short_cred.oa_base = (caddr_t)&(cache[i].sh);
	/* the short hand cred get serialized into a verifier */
	xdrmem_create(&xdrs, rqst->rq_xprt->xp_verf.oa_base,
	    SHORT_VERF_SIZE, XDR_ENCODE);
	if (! xdr_opaque_auth(&xdrs, &short_cred)) {
		stat = AUTH_BADCRED;
		goto done;
	}
	rqst->rq_xprt->xp_verf.oa_length = XDR_GETPOS(&xdrs);
	rqst->rq_xprt->xp_verf.oa_flavor = AUTH_SHORT;
done:
	XDR_DESTROY(&xdrs);
	return (stat);
}


/*
 * Shorthand unix authenticator
 * Looks up longhand in a cache.
 */
enum auth_stat 
_svcauth_short(rqst, msg)
	struct svc_req *rqst;
	struct rpc_msg *msg;
{
	short i;

	if (!inited) {
		svcauth_unix_init();
	}
	if (msg->rm_call.cb_cred.oa_length != SHORT_CRED_SIZE)
		return (AUTH_BADCRED);
	if ((i = find_short_hand(*(u_long *)msg->rm_call.cb_cred.oa_base)) < 0)
		return (AUTH_REJECTEDCRED);
	rqst->rq_clntcred = (caddr_t)&(cache[i].aup);
	return (AUTH_OK);
}


/*
 * returns cache index or -1 if sh not in the cache
 */
static short
find_short_hand(sh)
	register u_long sh;  /* short hand value */
{
	/* declared in order of importance */
	register short entry, i, c, p;

	queries++;
	for (c = cnt, i = head; c > 0; --c, i = nexti(i)) {

		entry = cacheindex[i];
		if (sh == cache[entry].sh) {
			/* cache hit! Now buble swap i up one notch */
			cache_hit(c, p);  /* used for accounting only */
			if (i != head) {
				/* c acts as the temporary variable */
				p = previ(i);
				c = cacheindex[p];
				cacheindex[p] = entry; /* gets cacheindex[i] */
				cacheindex[i] = c;
			}
			return (entry);
		}  /* end of successful cache hit */
	}
	return (-1);
}

/*
 * returns cache index or -1 if cred not in the cache
 */
static short
find_long_hand(cred_base, len)
	register caddr_t cred_base;
	register int len;
{
	/* declared in order of importance */
	register short entry, i, c, p;

	queries++;
	for (c = cnt, i = head; c > 0; --c, i = nexti(i)) {

		entry = cacheindex[i];
		if ((cache[entry].cred_len == len) &&
		    (bcmp(cache[entry].cred_base, cred_base, len) == 0)) {
			/* cache hit! Now buble swap i up one notch */
			cache_hit(c, p);  /* used for accounting only */
			if (i != head) {
				/* c acts as the temporary variable */
				p = previ(i);
				c = cacheindex[p];
				cacheindex[p] = entry; /* gets cacheindex[i] */
				cacheindex[i] = c;
			}
		return (entry);
		}  /* end of successful cache hit */
	}
	return (-1);
}

/*
 * Place a new entry at the HEAD of the cache.  This means moving the
 * heap index back one and possibly flushing the oldest entry from the cache.
 */
static
cache_new_user(base, len, aup)
	caddr_t base;
	int len;
	struct authunix_parms *aup;
{
	register short entry;
	struct timeval now;

	head = previ(head);
	entry = cacheindex[head];
	if (cnt == CACHE_SIZE) { /* full cache, delete lru entry */
		XDR xdrs;

		xdrs.x_op = XDR_FREE;
		deletions++;
		if (cache[entry].cred_base != NULL) {
			mem_free(cache[entry].cred_base,
			    cache[entry].cred_len);
			cache[entry].cred_base = NULL;
		}
		(void)xdr_authunix_parms(&xdrs, &cache[entry].aup);
	} else {
		cnt++;
	}
	/* now add current entry, raw cred must be copied */
	additions++;
	cache[entry].aup = *aup;
	cache[entry].cred_len = len;
	if ((cache[entry].cred_base = (char *)mem_alloc(len)) == NULL) {
		fprintf(stderr, "cache_new_user: out of memory\n");
		additions--;
		return;
	}
	bcopy(base, cache[entry].cred_base, (u_int)len);
	/* finally compute a new, unique short hand value */
	cache[entry].sh = ++ last_time.tv_sec;
	/* don't let real time get ahead of last_time */
	while (TRUE) {
		(void)gettimeofday(&now, (struct timezone *)0);
		if (((long int)now.tv_sec - (long int)last_time.tv_sec) > 0)
			break;
		sleep(1);
	}
}

/*
 * Initialize the shorthand cache.
 * Must be called before unix auth can be used!
 */
static svcauth_unix_init()
{
	register short i;

	inited++;
	(void)gettimeofday(&last_time, (struct timezone *)0);
	for (i = 0; i < CACHE_SIZE; ++i) {
		cacheindex[i] = i;
	}
}
