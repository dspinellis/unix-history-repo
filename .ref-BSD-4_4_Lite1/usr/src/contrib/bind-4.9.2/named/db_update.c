#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#)db_update.c	4.28 (Berkeley) 3/21/91";
static char rcsid[] = "$Id: db_update.c,v 4.9.1.6 1993/12/06 00:43:02 vixie Exp $";
#endif /* not lint */

/*
 * ++Copyright++ 1986, 1990
 * -
 * Copyright (c) 1986, 1990
 *    The Regents of the University of California.  All rights reserved.
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
 * 	This product includes software developed by the University of
 * 	California, Berkeley and its contributors.
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
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 */

#include <stdio.h>
#include <syslog.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>

#include "named.h"

static void			fixttl __P((struct databuf *));
static int			db_cmp __P((struct databuf *,
					    struct databuf *));

#ifdef CRED
/* int
 * isRefByNS(name, htp)
 *	recurse through all of `htp' looking for NS RR's that refer to `name'.
 * returns:
 *	nonzero if at least one such NS RR exists
 * cautions:
 *	this is very expensive; probably you only want to use on fcachetab.
 */
static int
isRefByNS(name, htp)
	char name[];
	struct hashbuf *htp;
{
	register struct namebuf *np;
	register struct databuf *dp;

	for (np = htp->h_tab[0];  np != NULL;  np = np->n_next) {
		for (dp = np->n_data;  dp != NULL;  dp = dp->d_next) {
			if ((dp->d_class == C_ANY || dp->d_class == C_IN) &&
			    (dp->d_type == T_NS) &&
#ifdef NCACHE
			    (!dp->d_rcode) &&
#endif
			    !strcasecmp(name, (char *)dp->d_data)) {
				return 1;
			}
		}
		if (np->n_hash && isRefByNS(name, np->n_hash)) {
			return 1;
		}
	}
	return 0;
}
#endif /*CRED*/

/* int
 * db_update(name, odp, newdp, flags, htp)
 *	update data base node at `name'.  `flags' controls the action.
 * side effects:
 *	inverse query tables modified, if we're using them.
 * return value:
 *	OK - success
 *	NONAME - name doesn't exist
 *	AUTH - you can't do that
 *	DATAEXISTS - there's something there and DB_NODATA was specified
 *	NODATA - there's no data, and (DB_DELETE or DB_MEXIST) was spec'd
 */
int
db_update(name, odp, newdp, flags, htp)
	char name[];
	struct databuf *odp, *newdp;
	int flags;
	struct hashbuf *htp;
{
	register struct namebuf *np;
	register struct databuf *dp, *pdp;
	char *fname;
        int foundRR = 0;
#ifdef CRED
	int isHintNS = isRefByNS(name, fcachetab);
#endif /*CRED*/

	dprintf(3, (ddt, "db_update(%s, 0x%x, 0x%x, 0%o, 0x%x)%s\n",
		    name, odp, newdp, flags, htp,
		    (odp && (odp->d_flags&DB_F_HINT)) ? " hint":"" ));
	np = nlookup(name, &htp, &fname, newdp != NULL);
	if (np == NULL || fname != name)
		return NONAME;

#ifdef CRED
	/* some special checks for root NS' A RR's */
	if (newdp && isHintNS && newdp->d_type == T_A) {
		/* obviously bogus addresses die here */
		if (
#ifdef NCACHE
		(!newdp->d_rcode) &&
#endif
			(((struct in_addr *)newdp->d_data)->s_addr == 0)) {
			syslog(LOG_INFO, "bogus (0.0.0.0) root A RR received");
			return AUTH;
		}
		/* upgrade credibility of additional data */
		if (newdp->d_cred == DB_C_ADDITIONAL) {
			dprintf(3, (ddt,
				    "upgrading credibility for A RR (%s)\n",
				    name));
			newdp->d_cred = DB_C_ANSWER;
			newdp->d_clev = 0;
		}
	}
#endif /*CRED*/

        /* Reflect certain updates in hint cache also... */
	/* Don't stick data we are authoritative for in hints. */
        if (!(flags & DB_NOHINTS) && (odp != NULL) &&
	    (htp != fcachetab) &&		/* vix@dec mar92 */
	    (odp->d_zone <= 0) && !(odp->d_flags & DB_F_HINT) &&
            ((name[0] == '\0' && odp->d_type == T_NS) ||
	     (odp->d_type == T_A
#ifdef CRED
	      && isHintNS
#endif /*CRED*/
	      )
	     )
	    )
        {
		dprintf(3, (ddt, "db_update: hint '%s' %d\n",
			    name, odp->d_ttl));
		dp = savedata(odp->d_class, odp->d_type, odp->d_ttl,
			odp->d_data, odp->d_size);
		dp->d_zone = DB_Z_CACHE;
		dp->d_flags = DB_F_HINT;
#ifdef CRED
		dp->d_cred = DB_C_CACHE;
		dp->d_clev = 0;
#endif /*CRED*/
		if (db_update(name,
			      dp, dp,
			      (flags|DB_NOHINTS),
			      fcachetab)
		    != OK) {
			dprintf(3, (ddt, "db_update: hint %x freed\n", dp));
			(void) free((char *)dp);
		}
        }

	if (odp != NULL) {
		pdp = NULL;
		for (dp = np->n_data; dp != NULL; ) {
			if (!match(dp, odp->d_class, odp->d_type)) {
				if ((dp->d_type == T_CNAME ||
				    odp->d_type == T_CNAME) &&
				    odp->d_mark == dp->d_mark &&
#ifdef NCACHE
				    /* neither the odp nor the new dp are
				     * negatively cached records...
				     */
				    (!dp->d_rcode) &&
				    (!odp->d_rcode) &&
#endif /*NCACHE*/
				    zones[odp->d_zone].z_type != Z_CACHE) {
					syslog(LOG_ERR,
				     "%s has CNAME and other data (illegal)\n",
					    name);
					dprintf(1, (ddt,
				    "db_update: %s: CNAME and more (%d, %d)\n",
						    name, odp->d_type,
						    dp->d_type));
				}
				goto skip;
			}
			dprintf(5, (ddt,
			       "db_update: flags = %#x, sizes = %d, %d (%d)\n",
				    flags, odp->d_size, dp->d_size,
				    db_cmp(dp, odp)));
			if (flags & DB_NOTAUTH && dp->d_zone) {
				dprintf(1, (ddt,
			     "[%s].%d attempted update to auth zone %d '%s'\n",
					    inet_ntoa(from_addr.sin_addr),
					    ntohs(from_addr.sin_port),
					    dp->d_zone,
					    zones[dp->d_zone].z_origin));
				return AUTH;
			}
#ifdef CRED
			if (newdp) {
				dprintf(4, (ddt,
		     "credibility for %s is %d(%d) from [%s].%d, is %d(%d) in cache\n",
					    *name? ".": name,
					    newdp->d_cred,
					    newdp->d_clev,
					    inet_ntoa(from_addr.sin_addr),
					    ntohs(from_addr.sin_port),
					    dp->d_cred,
					    dp->d_clev));
				if ((newdp->d_cred > dp->d_cred) || 
					((newdp->d_cred == DB_C_AUTH) && 
					(newdp->d_clev > dp->d_clev))) {
					/* better credibility and the old datum
					 * was not from a zone file.  remove
					 * the old datum.
					 */
					dp = rm_datum(dp, np, pdp);
					continue;
				}
				if (newdp->d_cred < dp->d_cred) {
					/* credibility is worse.  ignore it. */
					return AUTH;
				}
				/* credibility is the same.
				 * let it aggregate in the normal way.
				 */
			}
#endif /*CRED*/
			if ((flags & DB_NODATA) && !db_cmp(dp, odp)) {
				/* refresh ttl if cache entry */
				if (dp->d_zone == 0) {
					if (odp->d_zone != 0) {	/* XXX */
						/* changing cache->auth */
						dp->d_zone = odp->d_zone;
						dp->d_ttl = odp->d_ttl;
						dprintf(4, (ddt,
				    "db_update: cache entry now in auth zone\n"
							    ));
						return DATAEXISTS;
					}
					fixttl(odp);
					if (odp->d_ttl > dp->d_ttl)
						dp->d_ttl = odp->d_ttl;
					dprintf(3, (ddt,
						"db_update: new ttl %d, +%d\n",
						    dp->d_ttl,
						    dp->d_ttl - tt.tv_sec));
				}
				return DATAEXISTS;
			}
			/*
			 * If the old databuf has some data, check that the
			 * data matches that in the new databuf (so UPDATED
			 * will delete only the matching RR)
			 */
			if (odp->d_size > 0) {
				if (db_cmp(dp, odp))
					goto skip;
			}
			foundRR = 1;
			if (flags & DB_DELETE)
				dp = rm_datum(dp, np, pdp);
			else {
skip:				pdp = dp;
				dp = dp->d_next;
			}
		}
                if (!foundRR) {
			if (flags & DB_DELETE)
				return NODATA;
			if (flags & DB_MEXIST)
				return NODATA;
		}
	}
	if (newdp == NULL)
		return OK;
	fixttl(newdp);
	dprintf(3, (ddt, "db_update: adding%s %x\n",
		    (newdp->d_flags&DB_F_HINT) ? " hint":"", newdp));
#if INVQ
	if (!(newdp->d_flags & DB_F_HINT))
		addinv(np, newdp);	/* modify inverse query tables */
#endif

	/* Add to end of list, generally preserving order */
	newdp->d_next = NULL;
	if ((dp = np->n_data) == NULL)  {
#ifdef	DATUMREFCNT
		newdp->d_rcnt = 1;
#endif
		np->n_data = newdp;
		return OK;
	}
	/* XXX: need to check for duplicate WKS records and flag error */
	while (dp->d_next != NULL) {
		if ((flags & DB_NODATA) && !db_cmp(dp, newdp))
			return DATAEXISTS;
		dp = dp->d_next;
	}
	if ((flags & DB_NODATA) && !db_cmp(dp, newdp))
		return DATAEXISTS;
#ifdef	DATUMREFCNT
	newdp->d_rcnt = 1;
#endif
	dp->d_next = newdp;
	return OK;
}

static void
fixttl(dp)
	register struct databuf *dp;
{
	if (dp->d_zone == 0 && !(dp->d_flags & DB_F_HINT)) {
		if (dp->d_ttl <= tt.tv_sec)
			return;
		else if (dp->d_ttl < tt.tv_sec+min_cache_ttl)
			dp->d_ttl = tt.tv_sec+min_cache_ttl;
		else if (dp->d_ttl > tt.tv_sec+max_cache_ttl)
			dp->d_ttl = tt.tv_sec+max_cache_ttl;
	}
	return;
}

/*
 * Compare type, class and data from databufs for equivalence.
 * Must be case insensitive for some domain names.
 * Return 0 if equivalent, nonzero otherwise.
 */
static int
db_cmp(dp1, dp2)
	register struct databuf *dp1, *dp2;
{
	register u_char *cp1, *cp2;
	int len;

	if (dp1->d_type != dp2->d_type || dp1->d_class != dp2->d_class)
		return(1);
	if (dp1->d_size != dp2->d_size)
		return(1);
	if (dp1->d_mark != dp2->d_mark)
		return(1);		/* old and new RR's are distinct */
#ifdef NCACHE
	if (dp1->d_rcode && dp2->d_rcode)
		return((dp1->d_rcode == dp1->d_rcode)?0:1);
	if (dp1->d_rcode || dp2->d_rcode)
		return(1);
#endif

	switch (dp1->d_type) {

	case T_A:
	case T_UID:
	case T_GID:
	case T_WKS:
	case T_NULL:
#ifdef ALLOW_T_UNSPEC
        case T_UNSPEC:
#endif
		return bcmp(dp1->d_data, dp2->d_data, dp1->d_size);

	case T_NS:
	case T_CNAME:
	case T_PTR:
	case T_MB:
	case T_MG:
	case T_MR:
	case T_UINFO:
		return strcasecmp((char *)dp1->d_data, (char *)dp2->d_data);

	case T_HINFO:
		cp1 = dp1->d_data;
		cp2 = dp2->d_data;
		len = *cp1;
		if (strncasecmp((char *)++cp1, (char *)++cp2, len))
			return 1;
		cp1 += len;
		cp2 += len;
		len = *cp1;
		return strncasecmp((char *)++cp1, (char *)++cp2, len);

	case T_SOA:
	case T_MINFO:
	case T_RP:
		if (strcasecmp((char *)dp1->d_data, (char *)dp2->d_data))
			return(1);
		cp1 = dp1->d_data + strlen((char *)dp1->d_data) + 1;
		cp2 = dp2->d_data + strlen((char *)dp2->d_data) + 1;
		if (dp1->d_type != T_SOA)
			return strcasecmp((char *)cp1, (char *)cp2);
		if (strcasecmp((char *)cp1, (char *)cp2))
			return 1;
		cp1 += strlen((char *)cp1) + 1;
		cp2 += strlen((char *)cp2) + 1;
		return bcmp(cp1, cp2, sizeof(u_int32_t) * 5);
	
	case T_MX:
	case T_AFSDB:
		cp1 = dp1->d_data;
		cp2 = dp2->d_data;
		if (*cp1++ != *cp2++ || *cp1++ != *cp2++)	/* cmp prio */
			return 1;
		return strcasecmp((char *)cp1, (char *)cp2);

	case T_TXT:
		if (dp1->d_size != dp2->d_size)
			return 1;
		return bcmp(dp1->d_data, dp2->d_data, dp1->d_size);

	default:
		return (1);
	}
}
