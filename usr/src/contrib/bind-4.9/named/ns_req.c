#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#)ns_req.c	4.47 (Berkeley) 7/1/91";
static char rcsid[] = "$Id: ns_req.c,v 1.1 1993/06/01 02:33:47 vixie Exp vixie $";
#endif /* not lint */

/*
 * ++Copyright++ 1986, 1988, 1990
 * -
 * Copyright (c) 1986, 1988, 1990 Regents of the University of California.
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

#include <sys/param.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <syslog.h>
#include <errno.h>
#include <stdio.h>
#include <resolv.h>
#include "../conf/portability.h"
#include "../conf/options.h"
#include "ns.h"
#include "db.h"

#define NADDRECS	20

extern	int	debug;
extern	FILE	*ddt;

struct addinfo {
	char	*a_dname;		/* domain name */
	u_short	a_class;		/* class for address */
};

static	struct	addinfo addinfo[NADDRECS];	/* additional info records */
static	void	addname();

int	addcount;			/* number of names in addinfo */
int	xfr_disabled = 0;		/* set to disable zone xfrs */
int	needs_prime_cache = 0;		/* set if we need a priming */

u_char	*dnptrs[20];		/* ptrs to dnames in message for dn_comp */

extern int	writemsg();		/* db_glue.c */
extern void	fp_query();
extern int	dn_comp(), add_data();

extern int match();
extern void prime_cache();
extern time_t retrytime();
extern struct qinfo *sysquery();
#ifdef LOCALDOM
extern char *localdomain;
#endif
#ifdef XFRNETS
extern struct netinfo *xfrnets;
#endif

int stale(), make_rr(), doaddinfo(), doaddauth();
void ns_req(), fwritemsg(), getname(), doaxfr(), startxfr();

/*
 * Process request using database; assemble and send response.
 */
void
ns_req(msg, msglen, buflen, qsp, from, dfd)
	u_char *msg;
	int msglen, buflen;
	struct qstream *qsp;
	struct sockaddr_in *from;
	int dfd;
{
	register HEADER *hp;
	register u_char *cp;
	struct namebuf *np;
	register struct databuf *dp;
	struct hashbuf *htp;
	struct netinfo *lp;
	char *fname, *answers;
	u_char *eom, *omsg;
	char dnbuf[MAXDNAME], *dname;
	u_char **dpp;
	int n, class, type, count, foundname, founddata, omsglen, cname = 0;
	u_short id;
	struct databuf *nsp[NSMAX];
	struct qinfo *qp;
	extern struct netinfo *local();
	extern struct fwdinfo *fwdtab;

#ifdef DEBUG
	if (debug > 3) {
		fprintf(ddt,"ns_req()\n");
		fp_query((char *)msg, ddt);
	}
#endif

	hp = (HEADER *) msg;
	if (hp->qr) {
		ns_resp(msg, msglen);

		/* Now is a safe time for housekeeping */
		if (needs_prime_cache)
			prime_cache();
		return;
	}

	hp->rcode = NOERROR;
	cp = msg + sizeof(HEADER);
	eom = msg + msglen;
	dpp = dnptrs;
	*dpp++ = msg;
	addcount = 0;

	switch (hp->opcode) {
	case QUERY:
#ifdef STATS
		stats[S_QUERIES].cnt++;
#endif
		if (ntohs(hp->qdcount) != 1 ||
		    hp->ancount || hp->nscount || hp->arcount) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR Query header counts wrong\n");
#endif
			hp->qdcount = 0;
			hp->ancount = 0;
			hp->nscount = 0;
			hp->arcount = 0;
			hp->rcode = FORMERR;
			goto finish;
		}
		/*
		 * Get domain name, class, and type.
		 */
		if ((*cp & INDIR_MASK) == 0)
			*dpp++ = cp;	/* remember name for compression */
		*dpp = NULL;
		if ((n = dn_expand(msg, eom, cp, (u_char *)dnbuf,
		    sizeof(dnbuf))) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR Query expand name failed\n");
#endif
			hp->rcode = FORMERR;
			goto finish;
		}
		cp += n;
		GETSHORT(type, cp);
		GETSHORT(class, cp);
		if (cp > eom) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR Query message length short\n");
#endif
			hp->rcode = FORMERR;
			goto finish;
		}
#ifdef DEBUG
		if (cp < eom)
			if (debug > 5)
			    fprintf(ddt,"message length > received message\n");
#endif

#ifdef STATS
		if ((type > T_ANY) || (type < 0))
		    typestats[0]++;	/* Bad type */
		else
		    typestats[type]++;
#endif
		/*
		 * Process query.
		 */
		if (type == T_AXFR) {
			/* refuse request if not a TCP connection */
			if (qsp == QSTREAM_NULL || xfr_disabled) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt,
						"T_AXFR via UDP refused\n");
#endif
				goto refuse;
			}
#ifdef XFRNETS
			if (xfrnets) {
				/* if xfrnets was specified, peer address
				 * must be on it.  should probably allow
				 * for negation some day. (vix@decwrl)
				 */
				struct sockaddr saddr;
				int slen = (sizeof saddr);
				struct sockaddr_in *iaddr =
					(struct sockaddr_in *) &saddr;

				if (-1 == getpeername(qsp->s_rfd,
						      &saddr, &slen)) {
					syslog(LOG_WARNING, "getpeername: %m");
					goto refuse;
				}
				if (saddr.sa_family != AF_INET) {
					syslog(LOG_WARNING, "af%d???",
					       saddr.sa_family);
					goto refuse;
				}
				if (!net_on_netlist(iaddr->sin_addr,
						    xfrnets)) {
					syslog(LOG_INFO,
						"dangerous TCP addr: %s %u",
						inet_ntoa(iaddr->sin_addr),
						ntohs(iaddr->sin_port));
						goto refuse;
				}
			}
#endif /*XFRNETS*/
			dnptrs[0] = NULL;	/* don't compress names */
			hp->rd = 0;		/* recursion not possible */
		}
		buflen -= msglen;
		count = 0;
		foundname = 0;
		founddata = 0;
		dname = dnbuf;

#ifdef QRYLOG
		if (qrylog) {
			syslog(LOG_INFO, "XX /%s/%s/%s",
			       inet_ntoa(from->sin_addr), 
			       dname[0] == '\0' ? "." : dname, 
			       p_type(type));
		}
#endif /*QRYLOG*/

try_again:
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"req: nlookup(%s) id %d type=%d\n",
			    dname, hp->id, type);
#endif
		htp = hashtab;		/* lookup relative to root */
		if ((np = nlookup(dname, &htp, &fname, 0)) == NULL)
			fname = "";
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"req: %s '%s' as '%s' (cname=%d)\n",
				np == NULL ? "missed" : "found",
				dname, fname, cname);
#endif
#ifdef LOCALDOM
		/*
		 * if nlookup failed to find the name then
		 * see if there are any '.''s in the name
		 * if not then add local domain name to the
		 * name and try again.
		 */
		if (np == NULL && localdomain && strchr(dname, '.') == NULL) {
			(void) strcat(dname,".");
			(void) strcat(dname, localdomain);
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"req: nlookup(%s) type=%d\n",
				    dname, type);
#endif
			htp = hashtab;
			np = nlookup(dname, &htp, &fname, 0);
	        }
#endif /*LOCALDOM*/

#ifdef YPKLUDGE
		/* Some braindamaged resolver software will not 
		   recognize internet addresses in dot notation and 
		   send out address  queries for "names" such as 
		   128.93.8.1.  This kludge will prevent those 
		   from flooding higher level servers.
		   We simply claim to be authoritative and that
		   the domain doesn't exist.
		   Note that we could return the address but we
		   don't do that in order to encourage that broken
		   software is fixed.
		*/

		if (np==NULL && type==T_A && class == C_IN && dname)
		{
			struct sockaddr_in ina;

			if (!inet_aton(dname, &ina))
			{
				hp->rcode = NXDOMAIN;
				hp->aa = 1;
#ifdef DEBUG
				if (debug >= 3)
					fprintf(ddt, "ypkludge: hit as '%s'\n",
						dname);
#endif
				goto finish;
			}
		}
#endif /*YPKLUDGE*/

		if (np == NULL || fname != dname)
			goto fetchns;

		foundname++;
		answers = (char *)cp;
		count = cp - msg;
		n = finddata(np, class, type, hp, &dname, &buflen, &count);
		if (n == 0) {
			/* NO data available.  Refuse AXFR requests, or
			 * look for better servers for other requests.
			 */
			if (type == T_AXFR) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt,
						"T_AXFR refused: no data\n");
#endif
				goto refuse;
			} else
				goto fetchns;
		}
		cp += n;
		buflen -= n;
		msglen += n;
		hp->ancount += count;
		if (fname != dname && type != T_CNAME && type != T_ANY) {
			if (cname++ >= MAXCNAMES) {
#ifdef DEBUG
				if (debug >= 3)
				    fprintf(ddt,
					"resp: leaving, MAXCNAMES exceeded\n");
#endif
				hp->rcode = SERVFAIL;
				goto finish;
			}
			goto try_again;
		}
		founddata = 1;
#ifdef DEBUG
		if (debug >= 3) {
		    fprintf(ddt, "req: foundname = %d count = %d ",
			    foundname, count);
		    fprintf(ddt, "founddata = %d cname = %d\n",
			    founddata, cname);
		}
#endif
		if ((lp = local(from)) != NULL) 
			sort_response(answers, count, lp, cp);
		if (type == T_AXFR) {
			hp->ancount = htons(hp->ancount);
			startxfr(qsp, np, msg, cp - msg);
			sqrm(qsp);
			return;
		}
#ifdef notdef
		/*
		 * If we found an authoritative answer,
		 * we're done.
		 */
		if (hp->aa)
			goto finish;
#endif

fetchns:
		/*
	 	 * Look for name servers to refer to and fill in the authority
	 	 * section or record the address for forwarding the query
	 	 * (recursion desired).
	 	 */
		nsp[0] = NULL;
		count = 0;			/* del@harris */
		switch (findns(&np, class, nsp, &count)) {
		case NXDOMAIN:
			if (!foundname)
				hp->rcode = NXDOMAIN;
#ifdef DEBUG
			if (debug >= 3)
				fprintf(ddt,"req: leaving (%s, rcode %d)\n",
					dname, hp->rcode);
#endif
			if (class != C_ANY) {
				hp->aa = 1;
				/* XXX
				 * should return SOA if founddata == 0,
				 * but old named's are confused by an SOA
				 * in the auth. section if there's no error.
				 */
				if (foundname == 0 && np) {
				    n = doaddauth(hp, cp, buflen, np, nsp[0]);
				    cp += n;
				    buflen -= n;
				}
			}
			goto finish;

		case SERVFAIL:
			if (!founddata && !(forward_only && fwdtab)) {
				hp->rcode = SERVFAIL;
				goto finish;
			}
		}

		/*
		 *  If we successfully found the answer in the cache
		 *  or this is not a recursive query, then add the
		 *  nameserver references ("authority section") here
		 *  and return.
		 */
		if (founddata || (!hp->rd)) {
			n = add_data(np, nsp, cp, buflen);
			if (n < 0) {
				hp->tc = 1;
				n = (-n);
			}
			cp += n;
			buflen -= n;
			hp->nscount = htons((u_short)count);
			goto finish;
		}

		/*
		 *  At this point, we don't have the answer, but we do
		 *  have some NS's to try.  If the user would like us
		 *  to recurse, create the initial query.  If a cname
		 *  is involved, we need to build a new query and save
		 *  the old one in cmsg/cmsglen.
		 */
		if (cname) {
			omsg = (u_char *)malloc((unsigned)msglen);
			if (omsg == (u_char *)NULL) {
#ifdef DEBUG
				if (debug)
			        	fprintf(ddt,"ns_req: malloc fail\n");
#endif
				syslog(LOG_ERR, "ns_req: Out Of Memory");
				hp->rcode = SERVFAIL;
				break;
			}
			id = hp->id;
			hp->ancount = htons(hp->ancount);
			bcopy(msg, omsg, omsglen = msglen);
			msglen = res_mkquery(QUERY, dname, class, type,
				(char *)NULL, 0, NULL, (char *)msg,
				msglen+buflen);
		}
		n = ns_forw(nsp, msg, msglen, from, qsp, dfd, &qp, dname);
		if (n != FW_OK && cname)
			free(omsg);
		switch (n) {
		case FW_OK:
			if (cname) {
				qp->q_cname = cname;
				qp->q_cmsg = (char *)omsg;
				qp->q_cmsglen = omsglen;
				qp->q_id = id;
			}
			break;
		case FW_DUP:
			break;		/* Duplicate request dropped */
		case FW_NOSERVER:
			/* 
			** Don't go into an infinite loop if 
			** the admin gave root NS records in the cache
			** file without giving address records
			** for the root servers.
			*/
			if(np){
				if(np->n_dname[0] == '\0'){
#ifdef DEBUG
				    if (debug)
			        	fprintf(ddt,
				       "ns_req: no address for root server\n");
#endif
				    syslog(LOG_ERR, 
					 "ns_req: no address for root server");
				    hp->rcode = SERVFAIL;
				    goto finish;
				}
				np = np->n_parent;
			}
			goto fetchns;	/* Try again. */
		case FW_SERVFAIL:
			hp->rcode = SERVFAIL;
			goto finish;
		}
		/* Now is a safe time for housekeeping */
		if (needs_prime_cache)
			prime_cache();
		return;

#if INVQ
	case IQUERY: {
		register struct invbuf *ip;
		register int i;
		int dlen, alen;
		char anbuf[PACKETSZ], *data;

#ifdef STATS
		stats[S_IQUERIES].cnt++;
#endif
		hp->ancount = htons(hp->ancount);
		if (hp->ancount != 1 ||
		    hp->qdcount || hp->nscount || hp->arcount) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR IQuery header counts wrong\n");
#endif
			hp->qdcount = 0;
			hp->ancount = 0;
			hp->nscount = 0;
			hp->arcount = 0;
			hp->rcode = FORMERR;
			goto finish;
		}
		/*
		 * Skip domain name, get class, and type.
		 */
		if ((n = dn_skipname(cp, eom)) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR IQuery packet name problem\n");
#endif
			hp->rcode = FORMERR;
			goto finish;
		}
		cp += n;
		GETSHORT(type, cp);
		GETSHORT(class, cp);
		cp += sizeof(u_int32_t);
		GETSHORT(dlen, cp);
		cp += dlen;
		if (cp != eom) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR IQuery message length off\n");
#endif
			hp->rcode = FORMERR;
			goto finish;
		}
		/* not all inverse queries are handled. */
		switch (type) {
		case T_A:
		case T_UID:
		case T_GID:
			break;

		default:
			goto refuse;
		}
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"req: IQuery class %d type %d\n",
				class, type);
#endif
		fname = (char *)msg + sizeof(HEADER);
		bcopy(fname, anbuf, alen = (char *)cp - fname);
		data = anbuf + alen - dlen;
		cp = (u_char *)fname;
		buflen -= sizeof(HEADER);
		count = 0;
		for (ip = invtab[dhash(data, dlen)]; ip != NULL;
		    ip = ip->i_next) {
		    for (i = 0; i < INVBLKSZ; i++) {
			if ((np = ip->i_dname[i]) == NULL)
				break;
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"dname = %d\n", np->n_dname);
#endif
			for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
				if (!match(dp, class, type))
					continue;
				if (dp->d_size != dlen ||
				    bcmp(dp->d_data, data, dlen))
					continue;
				getname(np, dnbuf, sizeof(dnbuf));
#ifdef DEBUG
				if (debug > 2)
					fprintf(ddt,"req: IQuery found %s\n",
						dnbuf);
#endif
				buflen -= QFIXEDSZ;
				if ((n = dn_comp((u_char *)dnbuf, cp, buflen,
				    (u_char **)NULL, (u_char **)NULL)) < 0)
				{
					hp->tc = 1;
					goto finish;
				}
				cp += n;
				PUTSHORT((u_short)dp->d_type, cp);
				PUTSHORT((u_short)dp->d_class, cp);
				buflen -= n;
				count++;
			}
		    }
		}
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"req: IQuery %d records\n", count);
#endif
		hp->qdcount = htons((u_short)count);
		if (alen > buflen) {
			hp->tc = 1;
			break;
		}
		bcopy(anbuf, cp, alen);
		cp += alen;
		break;
	}
#endif /*INVQ*/

#ifdef ALLOW_UPDATES
/*
 * In a sense the following constant should be defined in <arpa/nameser.h>,
 * since it is returned here in place of a response code if the update was
 * forwarded, and the response codes are defined in nameser.h.  On the other
 * hand, though, this constant is only seen in this file.  The assumption
 * here is that none of the other return codes equals this one (a good
 * assumption, since they only occupy 4 bits over-the-wire)
 */
#define FORWARDED 1000
        /* Call InitDynUpdate for all dynamic update requests */
        case UPDATEM:
        case UPDATEMA:
        case UPDATED:
        case UPDATEDA:
        case UPDATEA:
                n = InitDynUpdate(hp, nsp, msg, msglen, cp, from, qsp, dfd);
                if (n == FORWARDED)
                        return; /* Return directly because InitDynUpdate
                                 * forwarded the query to the primary, so we
                                 * will send response later
                                 */
                else
                        break;  /* Either sucessful primary update or failure;
                                 * return response code to client
                                 */
#endif /* ALLOW_UPDATES */

	case ZONEREF:
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"Refresh Zone\n");
#endif
		/*FALLTHROUGH*/

	default:
#ifdef DEBUG
		if (debug >= 2)
			fprintf(ddt,"Opcode %d not implemented\n", hp->opcode);
#endif
		hp->qdcount = 0;
		hp->ancount = 0;
		hp->nscount = 0;
		hp->arcount = 0;
		hp->rcode = NOTIMP;
	}
	goto finish;
refuse:
	hp->rcode = REFUSED;
finish:
#ifdef STATS
	switch(hp->rcode) {
	case NOERROR:
		stats[S_RESPOK].cnt++;
		break;
	case FORMERR:
		stats[S_RESPFORMERR].cnt++;
		break;
	default:
		stats[S_RESPFAIL].cnt++;
		break;
	}
#endif
	hp->qr = 1;		/* set Response flag */
	hp->ra = 1;		/* Recursion is Available */
	hp->ancount = htons(hp->ancount);
	if (addcount) {
		n = doaddinfo(hp, cp, buflen);
		cp += n;
		buflen -= n;
	}

#ifdef DEBUG
	if (debug) {
	    fprintf(ddt,"req: answer -> %s %d (%d) id=%d %s\n",
	        inet_ntoa(from->sin_addr), 
		qsp == QSTREAM_NULL ? dfd : qsp->s_rfd, 
		ntohs(from->sin_port),
		ntohs(hp->id), local(from) == NULL ? "Remote" : "Local");
	}
	if (debug >= 10)
		fp_query((char *)msg, ddt);
#endif /* DEBUG */
	if (qsp == QSTREAM_NULL) {
		if (sendto(dfd, msg, cp-msg, 0, (struct sockaddr *)from,
		    sizeof(*from))< 0){
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"error returning msg errno=%d\n",errno);
#endif /* DEBUG */
		}
#ifdef STATS
		stats[S_OUTPKTS].cnt++;
#endif
	} else {
		(void) writemsg(qsp->s_rfd, msg, cp - msg);
		sq_done(qsp);
	}
	if (needs_prime_cache)
		prime_cache();	/* Now is a safe time */
}

void
fwritemsg(rfp, msg, msglen)
	FILE *rfp;
	char *msg;
	int msglen;
{
	u_short len = htons((u_short)msglen);

	if (fwrite((char *)&len, sizeof(len), 1, rfp) != 1 ||
	    fwrite(msg, msglen, 1, rfp) != 1) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"fwrite failed %d\n", errno);
#endif
	}
}

/*
 *  Test a datum for validity and return non-zero if it is out of date.
 */
int
stale(dp)
	register struct databuf *dp;
{
	register struct zoneinfo *zp = &zones[dp->d_zone];

	switch (zp->z_type) {

	case Z_PRIMARY:
		return (0);

	case Z_SECONDARY:
		/*
		 * Check to see whether a secondary zone
		 * has expired; if so clear authority flag
		 * for zone and return true.  If lastupdate
		 * is in the future, assume zone is up-to-date.
		 */
		if ((int32_t)(tt.tv_sec - zp->z_lastupdate) > (int32_t)zp->z_expire){
#ifdef DEBUG
			if (debug)
				fprintf(ddt,
					"stale: secondary zone %s expired\n",
					zp->z_origin);
#endif
			if (!haveComplained(zp->z_origin, (char*)stale)) {
				syslog(LOG_ERR,
				       "secondary zone \"%s\" expired",
				       zp->z_origin);
			}
			zp->z_state &= ~Z_AUTH;
			return (1);
		}
		return (0);

	case Z_CACHE:
#ifdef DEBUG
		if (debug >= 3)
			fprintf(ddt,"stale: ttl %d %d (x%x)\n",
				dp->d_ttl, dp->d_ttl - tt.tv_sec, dp->d_flags);
#endif
		if (dp->d_flags & DB_F_HINT)
			return(0);
		return(dp->d_ttl < tt.tv_sec);

	}
	abort();
	/* NOTREACHED */
}

/*
 * Copy databuf into a resource record for replies.
 * Return size of RR if OK, -1 if buffer is full.
 */
int
make_rr(name, dp, buf, buflen, doadd)
	char *name;
	register struct databuf *dp;
	u_char *buf;
	int buflen, doadd;
{
	register u_char *cp;
	u_char *cp1, *sp;
	struct zoneinfo *zp;
	register int32_t n;
	register int32_t ttl;
	u_char **edp = dnptrs + sizeof(dnptrs)/sizeof(dnptrs[0]);

#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"make_rr(%s, %x, %x, %d, %d) %d zone %d ttl %d\n",
			name, dp, buf,
			buflen, doadd, dp->d_size, dp->d_zone, dp->d_ttl);
#endif

	zp = &zones[dp->d_zone];
	/* check for outdated RR before updating dnptrs by dn_comp() (???) */
	if (zp->z_type == Z_CACHE) {
		ttl = dp->d_ttl - (u_int32_t) tt.tv_sec;
		if ((dp->d_flags & DB_F_HINT) || (ttl < 0)) {
#ifdef DEBUG
/*XXX*/if (debug >= 3) fprintf(ddt,"make_rr: %d=>0, x%x\n", ttl, dp->d_flags);
#endif
			ttl = 0;
		}
	} else {
		if (dp->d_ttl)
			ttl = dp->d_ttl;
		else
			ttl = zp->z_minimum;		/* really default */
#ifdef notdef /* don't decrease ttl based on time since verification */
		if (zp->z_type == Z_SECONDARY) {
			/*
			 * Set ttl to value received from primary,
			 * less time since we verified it (but never
			 * less than a small positive value).
			 */
			ttl -= tt.tv_sec - zp->z_lastupdate;
			if (ttl <= 0)
				ttl = 120;
		}
#endif
	}

	buflen -= RRFIXEDSZ;
	if ((n = dn_comp((u_char *)name, (u_char *)buf, buflen,
	    (u_char **)dnptrs, (u_char **)edp)) < 0)
		return (-1);
	cp = buf + n;
	buflen -= n;
	PUTSHORT((u_short)dp->d_type, cp);
	PUTSHORT((u_short)dp->d_class, cp);
	PUTLONG(ttl, cp);
	sp = cp;
	cp += sizeof(u_short);
	switch (dp->d_type) {
	case T_CNAME:
	case T_MG:
	case T_MR:
	case T_PTR:
		if ((n = dn_comp((u_char *)dp->d_data, (u_char *)cp, buflen,
		    (u_char **)dnptrs, (u_char **)edp)) < 0)
			return (-1);
		PUTSHORT((u_short)n, sp);
		cp += n;
		break;

	case T_MB:
	case T_NS:
		/* Store domain name in answer */
		if ((n = dn_comp((u_char *)dp->d_data, (u_char *)cp, buflen,
		    (u_char **)dnptrs, (u_char **)edp)) < 0)
			return (-1);
		PUTSHORT((u_short)n, sp);
		cp += n;
		if (doadd)
			addname((char*)dp->d_data, dp->d_class);
		break;

	case T_SOA:
	case T_MINFO:
	case T_RP:
		cp1 = dp->d_data;
		if ((n = dn_comp((u_char *)cp1, (u_char *)cp, buflen,
		    (u_char **)dnptrs, (u_char **)edp)) < 0)
			return (-1);
		cp += n;
		buflen -= dp->d_type == T_SOA ? n + 5 * sizeof(u_int32_t) : n;
		cp1 += strlen((char *)cp1) + 1;
		if ((n = dn_comp((u_char *)cp1, (u_char *)cp, buflen,
		    (u_char **)dnptrs, (u_char **)edp)) < 0)
			return (-1);
		cp += n;
		if (dp->d_type == T_SOA) {
			cp1 += strlen((char *)cp1) + 1;
			bcopy(cp1, cp,
			   (int)(n = dp->d_size - (cp1 - dp->d_data)));
			cp += n;
		}
		n = (u_short)((cp - sp) - sizeof(u_short));
		PUTSHORT((u_short)n, sp);
		break;

	case T_MX:
		/* cp1 == our data/ cp == data of RR */
		cp1 = dp->d_data;

 		/* copy preference */
 		bcopy(cp1,cp,sizeof(u_short));
 		cp += sizeof(u_short);
 		cp1 += sizeof(u_short);
 		buflen -= sizeof(u_short);

  		if ((n = dn_comp((u_char *)cp1, (u_char *)cp, buflen,
		    (u_char **)dnptrs, (u_char **)edp)) < 0)
  			return(-1);
  		cp += n;

  		/* save data length */
		n = (u_short)((cp - sp) - sizeof(u_short));
  		PUTSHORT((u_short)n, sp);
		if (doadd)
			addname((char*)cp1, dp->d_class);
		break;

	default:
		if (dp->d_size > buflen)
			return (-1);
		bcopy(dp->d_data, cp, dp->d_size);
		PUTSHORT((u_short)dp->d_size, sp);
		cp += dp->d_size;
	}
	return (cp - buf);
}

static void
addname(name, class)
	register char	*name;
	short		class;
{
	register struct addinfo *ap;
	register int n;

	for (ap = addinfo, n = addcount; --n >= 0; ap++)
		if (strcasecmp(ap->a_dname, name) == 0)
			return;
	/* add domain name to additional section */
	if (addcount < NADDRECS) {
		addcount++;
		ap->a_dname = name;
		ap->a_class = class;
	}
}

/*
 * Lookup addresses for names in addinfo and put into the message's
 * additional section.
 */
int
doaddinfo(hp, msg, msglen)
	HEADER *hp;
	u_char *msg;
	int msglen;
{
	register struct namebuf *np;
	register struct databuf *dp;
	register struct addinfo *ap;
	register u_char *cp;
	struct hashbuf *htp;
	char *fname;
	int n, count;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt, "doaddinfo() addcount = %d\n", addcount);
#endif

	if (hp->tc) {
#ifdef DEBUG
		if (debug >= 4)
			fprintf(ddt, "doaddinfo(): tc already set, bailing\n");
#endif
		return 0;
	}

	count = 0;
	cp = msg;
	for (ap = addinfo; --addcount >= 0; ap++) {
		int	foundstale = 0,
			foundany = 0,
			save_count = count,
			save_msglen = msglen;
		u_char	*save_cp = cp;
#ifdef DEBUG
		if (debug >= 3)
			fprintf(ddt,"do additional '%s'\n", ap->a_dname);
#endif
		htp = hashtab;	/* because "nlookup" stomps on arg. */
		np = nlookup(ap->a_dname, &htp, &fname, 0);
		if (np == NULL || fname != ap->a_dname)
			goto next_rr;
#ifdef DEBUG
		if (debug >= 3)
			fprintf(ddt,"found it\n");
#endif
		/* look for the data */
		for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
			if (!match(dp, (int)ap->a_class, T_A))
				continue;
			foundany++;
			if (stale(dp)) {
				foundstale++;
#ifdef DEBUG
				if (debug)
				    fprintf(ddt,
					    "doaddinfo: stale entry '%s'%s\n",
					    np->n_dname,
					    (dp->d_flags&DB_F_HINT)
					        ? " hint"
					        : ""
					    );
#endif
				continue;
			}
			/*
			 *  Should be smart and eliminate duplicate
			 *  data here.	XXX
			 */
			if ((n = make_rr(ap->a_dname, dp, cp, msglen, 0)) < 0){
				/* truncation in the additional-data section
				 * is not all that serious.  we do not set TC,
				 * since the answer and authority sections are
				 * OK; however, since we're not setting TC we
				 * have to make sure that none of the RR's for
				 * this name go out (!TC implies that all
				 * {name,type} appearances are complete -- and
				 * since we only do A RR's here, the name is
				 * the key).	vixie, 23apr93
				 */
				cp = save_cp;
				msglen = save_msglen;
				count = save_count;
				break;
			}
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,
				       "addinfo: adding address data n = %d\n",
					n);
#endif
			cp += n;
			msglen -= n;
			count++;
		}
next_rr:	if (foundstale) {
			/* Cache invalidate the address RR's */
			delete_all(np, (int)ap->a_class, T_A);
		}
		if (foundstale || !foundany) {
			/* ask a real server for this info */
			(void) sysquery(ap->a_dname, (int)ap->a_class, T_A);
		}
	}
	hp->arcount = htons((u_short)count);
	return (cp - msg);
}

int
doaddauth(hp, cp, buflen, np, dp)
	register HEADER *hp;
	u_char *cp;
	int buflen;
	struct namebuf *np;
	struct databuf *dp;
{
	char dnbuf[MAXDNAME];
	int n;

	getname(np, dnbuf, sizeof(dnbuf));
	if (stale(dp) || (n = make_rr(dnbuf, dp, cp, buflen, 1)) <= 0) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt, "doaddauth: can't add '%s' (%d) (n=%d)\n",
				dnbuf, buflen, n);
#endif
		if (n < 0) {
			hp->tc = 1;
		}
		return(0);
	} else {
		hp->nscount = htons(1);
		return(n);
	}
}


/*
 * Get the domain name of 'np' and put in 'buf'.  Bounds checking is done.
 */
void
getname(np, buf, buflen)
	struct namebuf *np;
	char *buf;
	int buflen;
{
	register char *cp;
	register int i;

	cp = buf;
	while (np != NULL) {
		if ((i = strlen(np->n_dname))+1 >= buflen) {
			*cp = '\0';
			syslog(LOG_ERR, "domain name too long: %s...\n", buf);
			strcpy(buf, "Name_Too_Long");
			return;
		}
		if (cp != buf)
			*cp++ = '.';
		(void) strcpy(cp, np->n_dname);
		cp += i;
		buflen -= (i+1);
		np = np->n_parent;
	}
	*cp = '\0';
}

/*
 * Do a zone transfer (or a recursive part of a zone transfer).
 * SOA record already sent.
 *
 * top always refers to the domain at the top of the zone being transferred.
 * np refers to a domain inside the zone being transferred,
 *	which will be equal to top if this is the first call,
 *	or will be a subdomain below top if this is a recursive call,
 * rfp is a stdio file to which output is sent.
 */
void
doaxfr(np, rfp, top)
	register struct namebuf *np;
	FILE *rfp;
	struct namebuf *top;
{
	register struct databuf *dp;
	register int n;
	struct hashbuf *htp;
	struct databuf *gdp;	/* glue databuf */
	struct namebuf *gnp;	/* glue namebuf */
	struct namebuf *tnp;	/* top namebuf */
	struct databuf *tdp;	/* top databuf */
	struct namebuf **npp, **nppend;
	char msg[PACKETSZ];
	u_char *cp;
	char *fname;
	char dname[MAXDNAME];
	HEADER *hp = (HEADER *) msg;
	int fndns;

#ifdef DEBUG
	if (debug && (np == top))
		fprintf(ddt,"doaxfr()\n");
#endif
	fndns = 0;
	hp->id = 0;
	hp->opcode = QUERY;
	hp->aa = hp->tc = hp->ra = hp->pr = hp->rd = 0;
	hp->qr = 1;
	hp->rcode = NOERROR;
	hp->qdcount = 0;
	hp->ancount = htons(1);
	hp->nscount = 0;
	hp->arcount = 0;
	cp = (u_char *) (msg + sizeof(HEADER));
	getname(np, dname, sizeof(dname));

	/* first do the NS records (del@harris) */
	for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
	    if (dp->d_type == T_NS) {
		fndns = 1;
		if ((n = make_rr(dname, dp, cp,
				 sizeof(msg)-sizeof(HEADER), 0)) < 0)
			continue;
		fwritemsg(rfp, msg, n + sizeof(HEADER));
		if (np != top) {
		    /*  Glue the sub domains together by sending 
		     *  the address records for the sub domain
		     *  name servers along if necessary.
		     *  Glue is necessary if the server is in any zone
		     *  delegated from the current (top) zone.  Such
		     *  a delegated zone might or might not be that
		     *  referred to by the NS record now being handled.
		     */
 		    htp = hashtab;
		    cp = (u_char *) (msg + sizeof(HEADER));
 		    gnp = nlookup((caddr_t)dp->d_data, &htp, &fname, 0);
 		    if (gnp == NULL || fname != (caddr_t)dp->d_data)
 			continue;
		    for (tnp = gnp; tnp != NULL; tnp = tnp->n_parent)
			if ( tnp == top )
			    break;
		    if ( tnp == NULL )
			continue;  /* name server is not below top domain */
		    for (tnp = gnp; tnp != top; tnp = tnp->n_parent) {
			for (tdp = tnp->n_data; tdp != NULL; tdp = tdp->d_next)
			    if (tdp->d_type == T_NS)
				break;
			if (tdp != NULL)
			    break; /* found a zone cut */
		    }
		    if (tnp == top)
			continue;  /* name server is not in a delegated zone */
		    /* now we know glue records are needed.  send them. */
 		    for(gdp=gnp->n_data; gdp != NULL; gdp=gdp->d_next) {
 			if (gdp->d_type != T_A || stale(gdp))
 			    continue;
 			if ((n = make_rr(fname, gdp, cp,
 		    	    sizeof(msg)-sizeof(HEADER), 0)) < 0)
	 		    continue;
 			fwritemsg(rfp, msg, n + sizeof(HEADER));
 		    }
		}
	    }
	}
	/* no need to send anything else because of delegation */
	if ((np != top) && fndns)
		return;

	/* do the rest of the data records */
	for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
		/*
		 * Skip the top SOA record (marks end of data);
		 * don't send SOA for subdomains, as we're not sending them;
		 * skip the NS records because we did them first.
		 */
		if (dp->d_type == T_SOA || dp->d_type == T_NS)
			continue;
		if (dp->d_zone == 0 || stale(dp))
			continue;
		if ((n = make_rr(dname, dp, cp, sizeof(msg)-sizeof(HEADER), 0)) < 0)
			continue;
		fwritemsg(rfp, msg, n + sizeof(HEADER));
	}

	/* Finally do non-delegated subdomains.  Delegated subdomains
	 * have already been handled.
	 */
	/*
	 * We find the subdomains by looking in the hash table for this
	 * domain, but the root domain needs special treatment, because
	 * of the following wart in the database design:
	 *
	 * The top level hash table (pointed to by the global `hashtab'
	 * variable) contains pointers to the namebuf's for the root as
	 * well as for the top-level domains below the root, in contrast
	 * to the usual situation where a hash table contains entries
	 * for domains at the same level.  The n_hash member of the
	 * namebuf for the root domain is NULL instead of pointing to a
	 * hashbuf for the top-level domains.  The n_parent members of
	 * the namebufs for the top-level domains are NULL instead of
	 * pointing to the namebuf for the root.
	 *
	 * We work around the wart as follows:
	 *
	 * If we are not dealing with the root zone then we just set
	 * htp = np->n_hash, pointing to the hash table for the current
	 * domain, and we walk through the hash table as usual,
	 * processing the namebufs for all the subdomains.
	 *
	 * If we are dealing with the root zone, then we set
	 * htp = hashtab, pointing to the global hash table (because
	 * there is no hash table associated with the root domain's
	 * namebuf.  While we walk this hash table, we take care not to
	 * recursively process the entry for the root namebuf.
	 *
	 * (apb@und nov1990)
	 */
	htp = ((dname[0] == '\0') ? hashtab : np->n_hash);
	if (htp == NULL) {
		return; /* no subdomains */
	}
	npp = htp->h_tab;
	nppend = npp + htp->h_size;
	while (npp < nppend) {
		for (np = *npp++; np != NULL; np = np->n_next) {
		    if (np->n_dname[0] != '\0') { /* don't redo root domain */
			doaxfr(np, rfp, top);
		    }
		}
	}
#ifdef DEBUG
	if (debug && (np == top))
		fprintf(ddt,"exit doaxfr()\n");
#endif
}

#ifdef ALLOW_UPDATES
/*
 * Called by UPDATE{A,D,DA,M,MA} to initiate a dynamic update.  If this is the
 * primary server for the zone being updated, we update the zone's serial
 * number and then call doupdate directly. If this is a secondary, we just
 * forward the update; this way, if the primary update fails (e.g., if the
 * primary is unavailable), we don't update the secondary; if the primary
 * update suceeds, ns_resp will get called with the response (when it comes
 * in), and then update the secondary's copy.
 */
InitDynUpdate(hp, nsp, msg, msglen, startcp, from, qsp, dfd)
	register HEADER *hp;
	struct databuf *nsp[];
	char *msg;
	int msglen;
	u_char *startcp;
	struct sockaddr_in *from;
	struct qstream *qsp;
	int dfd;
{
	struct zoneinfo *zp;
	char dnbuf[MAXDNAME];
	struct hashbuf *htp = hashtab;	/* lookup relative to root */
	struct namebuf *np;
	struct databuf *olddp, *newdp, *dp;
	struct databuf **nspp;
	char *fname;
	register u_char *cp = startcp;
	short class, type;
	int n, size, zonenum;
	char ZoneName[MAXDNAME], *znp;

	if ((n = dn_expand(msg, msg + msglen, cp, dnbuf, sizeof(dnbuf))) < 0) {
#ifdef DEBUG
		if (debug)
		    fprintf(ddt,"FORMERR InitDynUpdate expand name failed\n");
#endif
		hp->rcode = FORMERR;
		return(FORMERR);
	}
	cp += n;
	GETSHORT(type, cp);
	if (type == T_SOA) {	/* T_SOA updates not allowed */
		hp->rcode = REFUSED;
#ifdef DEBUG
		if (debug)
		    fprintf(ddt, "InitDynUpdate: REFUSED - SOA update\n");
#endif
		return(REFUSED);
	}
	GETSHORT(class, cp);
	cp += sizeof(u_int32_t);
	GETSHORT(size, cp);
/****XXX - need bounds checking here ****/
	cp += size;

	if ((zonenum = findzone(dnbuf, class)) == 0) {  /* zone not found */
		hp->rcode = NXDOMAIN;
		return(NXDOMAIN);
	}
	zp = &zones[zonenum];

	/* Disallow updates for which we aren't authoratative.  Note: the
	   following test doesn't work right:  If it's for a non-local zone,
	   we will think it's a primary but be unable to lookup the namebuf,
	   thus returning 'NXDOMAIN' */
	if (zp->z_type != Z_PRIMARY && zp->z_type != Z_SECONDARY) {
		hp->rcode = REFUSED;
#ifdef DEBUG
		if (debug)
		    fprintf(ddt, "InitDynUpdate: REFUSED - non-primary, non-sedondary update\n");
#endif
		return(REFUSED);
	}
	if (!(zp->z_state & Z_DYNAMIC)) {
		hp->rcode = REFUSED;
#ifdef DEBUG
		if (debug)
		    fprintf(ddt, "InitDynUpdate: REFUSED - dynamic flag not set for zone\n");
#endif
		return(REFUSED);
	}

	/*
	 * Lookup the zone namebuf.  Lookup "xyz" not "xyz.", since
	 * otherwise the lookup fails, because '.' may have a nil n_hash
	 * associated with it.
	 */
	strcpy(ZoneName, zp->z_origin);
	znp = &ZoneName[strlen(ZoneName) - 1];
	if (*znp == '.')
		*znp = NULL;
	np = nlookup(ZoneName, &htp, &fname, 0);
	if ((np == NULL) || (fname != ZoneName)) {
#ifdef DEBUG
		if (debug)
		    fprintf(ddt, "InitDynUpdate: lookup failed on zone (%s)\n",
			    ZoneName);
#endif /* DEBUG */
	        syslog(LOG_ERR, "InitDynUpdate: lookup failed on zone (%s)\n",
		       ZoneName);
		hp->rcode = NXDOMAIN;
		return(NXDOMAIN);
	}

	/*
	 * If this is the primary copy increment the serial number.  Don't
	 * increment the serial number if this is a secondary; this way, if 2
	 * different secondaries both update the primary, they will both have
	 * lower serial numbers than the primary has, and hence eventually
	 * refresh and get all updates and become consistent.
	 *
	 * Note that the serial number must be incremented in both the zone
	 * data structure and the zone's namebuf.
	 */
	switch (zp->z_type) {
	case Z_SECONDARY:		/* forward update to primary */
		nspp = nsp;
		dp = np->n_data;
		while (dp != NULL) {
			if (match(dp, class, T_NS)) {
				if (nspp < &nsp[NSMAX-1])
					*nspp++ = dp;
				else
					break;
			}
			dp = dp->d_next;
		}
		*nspp = NULL; /* Delimiter */
		if (ns_forw(nsp, msg, msglen, from, qsp, dfd, NULL, dnbuf)
		    <
		    0) {
			hp->rcode = SERVFAIL;
			return(SERVFAIL);
		}
		return(FORWARDED);

	case Z_PRIMARY:
		zp->z_serial++;
		olddp = np->n_data; /* old databuf */
		/* Find the SOA record */
		for (olddp = np->n_data; olddp != NULL; olddp = olddp->d_next)
			if (match(olddp, class, T_SOA))
				break;
		if (olddp == NULL) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"InitDynUpdate: Couldn't find SOA record for '%s'\n",
				        ZoneName);
#endif /* DEBUG */
			syslog(LOG_ERR,
			   "InitDynUpdate: Couldn't find SOA record for '%s'\n"
,
			   ZoneName);
			hp->rcode = NXDOMAIN;
			return(NXDOMAIN);
		}
		newdp = savedata(olddp->d_class, olddp->d_type, olddp->d_ttl,
				 olddp->d_data, olddp->d_size);
		newdp->d_zone = olddp->d_zone;
#ifdef CRED
		newdp->d_cred = DB_C_AUTH;
#endif
		cp = (u_char *)newdp->d_data;
		cp += strlen(cp) + 1; /* skip origin string */
		cp += strlen(cp) + 1; /* skip in-charge string */
		putlong((u_int32_t)(zp->z_serial), cp);
#ifdef DEBUG
		if (debug >= 4) {
			fprintf(ddt, "after stuffing data into newdp:\n");
			printSOAdata(newdp);
		}
#endif /* DEBUG */

		if ((n = db_update(ZoneName, olddp, newdp, DB_DELETE,
				   hashtab)) != NOERROR) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"InitDynUpdate: SOA update failed\n");
#endif /* DEBUG */
			hp->rcode = NOCHANGE;
			free((char*) dp);		/* vix@dec mar92 */
			return(NOCHANGE);
		}

		/* Now update the RR itself */
		if (doupdate(msg, msglen, msg + sizeof(HEADER),
			     zonenum, (struct databuf *)0, DB_NODATA
#ifdef CRED
			     , DB_C_AUTH
#endif
			     ) < 0) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"InitDynUpdate: doupdate failed\n");
#endif /* DEBUG */
			/* doupdate fills in rcode */
			return(hp->rcode);
		}
		zp->z_state |= Z_CHANGED;
		return(NOERROR);
	}
}

#ifdef DEBUG
/*
 * Print the contents of the data in databuf pointed to by dp for an SOA record
 */
printSOAdata(dp)
	struct databuf *dp;
{
	register u_char *cp;

	if (!debug)
		return;  /* Otherwise fprintf to ddt will bomb */
	cp = (u_char *)dp->d_data;
	fprintf(ddt, "printSOAdata(%x): origin(%x)='%s'\n", dp, cp, cp);
	cp += strlen(cp) + 1; /* skip origin string */
	fprintf(ddt, "printSOAdata: in-charge(%x)='%s'\n", cp, cp);
	cp += strlen(cp) + 1; /* skip in-charge string */
	fprintf(ddt, "printSOAdata: serial(%x)=%d\n", cp, _getlong(cp));
}
#endif /* DEBUG */
#endif /* ALLOW_UPDATES */

/* rm_datum(dp, np, pdp)
 *	remove datum 'dp' from name 'np'.  pdp is previous data pointer.
 * return value:
 *	"next" field from removed datum, suitable for relinking
 */
struct databuf *
rm_datum(dp, np, pdp)
	register struct databuf *pdp, *dp;
	register struct namebuf *np;
{
	register struct databuf *ndp = dp->d_next;

#ifdef DEBUG
	if (debug > 2)
		fprintf(ddt, "rm_datum(%x, %x, %x) -> %x\n",
			dp, np->n_data, pdp, ndp);
#endif /* DEBUG */
#if INVQ
	rminv(dp);
#endif
	if (pdp == NULL)
		np->n_data = ndp;
	else
		pdp->d_next = ndp;
	free((char *)dp);
	return ndp;
}

/* rm_name(np, he, pnp)
 *	remove name 'np' from parent 'pp'.  pnp is previous name pointer.
 * return value:
 *	"next" field from removed name, suitable for relinking
 */
struct namebuf *
rm_name(np, pp, pnp)
	struct namebuf *np, **pp, *pnp;
{
	struct namebuf *nnp = np->n_next;
	char *msg;

	/* verify */
	if ( (np->n_data && (msg = "data"))
	  || (np->n_hash && (msg = "hash"))
	    ) {
		fprintf(ddt,
			"rm_name(%x(%s)): non-nil %s pointer\n",
			np, np->n_dname?np->n_dname:"Nil", msg);
		syslog(LOG_ERR,
			"rm_name(%x(%s)): non-nil %s pointer\n",
			np, np->n_dname?np->n_dname:"Nil", msg);
		abort();
	}

	/* unlink */
	if (pnp) {
		pnp->n_next = nnp;
	} else {
		*pp = nnp;
	}

	/* deallocate */
	free(np->n_dname);
	free((char*) np);

	/* done */
	return nnp;
}

void
startxfr(qsp, np, msg, msglen)
	struct qstream *qsp;
	struct namebuf *np;
	char	*msg;
	int	msglen;
{
	register FILE *rfp;
	int fdstat;

#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"startxfr()\n");
#endif
	/*
	 * child does the work while
	 * the parent continues
	 *
	 * XXX this needs to be a vfork/exec since on non-copy-on-write
	 * systems with huge nameserver images, this is very expensive.
	 */
	if (fork() == 0) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"startxfr: child pid %d\n", getpid());
#endif
		rfp = fdopen(qsp->s_rfd, "w");
		setproctitle("zone XFR to", qsp->s_rfd);
		fdstat = fcntl(qsp->s_rfd, F_GETFL, 0);
		if (fdstat != -1)
			(void) fcntl(qsp->s_rfd, F_SETFL,
				     fdstat & ~PORT_NONBLOCK);
		fwritemsg(rfp, msg, msglen);
		doaxfr(np, rfp, np);
		fwritemsg(rfp, msg, msglen);
		(void) fflush(rfp);
		exit(0);
	}
}
