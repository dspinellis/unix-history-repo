#ifndef lint
static char sccsid[] = "@(#)ns_resp.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <syslog.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

extern int errno;
extern char *dnptrs[];
extern char *malloc();
struct	databuf *nsp[MAXNS], **nspp;

/*
 * Handle a response from a forwarded query.
 */
ns_resp(msg, msglen)
	char *msg;
	int msglen;
{
	register struct qinfo *qp;
	register HEADER *hp;
	register char *cp, *tp;
	int i, c, n, ancount, nscount, arcount;
	int type;
	int qtype, qclass;
	int cname = 0; /* flag for processing cname response */
	int count, founddata;
	int buflen;
	int newmsglen;
	char name[MAXDNAME], *dname;
	char *fname;
	char *newmsg;

	struct hashbuf *htp;
	struct databuf *dp, *tmp, *pdp;
	struct namebuf *np;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"ns_resp(%x, %d)\n", msg, msglen);
#endif
	hp = (HEADER *) msg;
	if( (qp = qfindid(hp->id)) == NULL )
		return;

	if (hp->rcode != NOERROR || hp->opcode != QUERY)
		goto retmsg;
	/*
	 * Skip query section
	 */
	cp = msg + sizeof(HEADER);
	if (hp->qdcount) {
		if ((n = dn_skip(cp)) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR ns_resp() dn_skip failed\n");
#endif
			hp->rcode = FORMERR;
			goto retmsg;
		}
		cp += n;
		qtype = getshort(cp);
		cp += sizeof(u_short);
		qclass = getshort(cp);
		cp += sizeof(u_short);
	}
	/*
	 * Save answers, name server, and additional records for future use.
	 */
	ancount = ntohs(hp->ancount);
	nscount = ntohs(hp->nscount);
	arcount = ntohs(hp->arcount);
	if (ancount == 1 || nscount) {
		/*
	 	 *  Check if it's a CNAME responce
	 	 */
		tp = cp;
		tp += dn_skip(tp); /* name */
		type = getshort(tp);
		tp += sizeof(u_short); /* type */
		if (type == T_CNAME) {
			tp += sizeof(u_short); /* class */
			tp += sizeof(u_long);  /* ttl */
			tp += sizeof(u_short); /* dlen */
			cname++;
#ifdef DEBUG
			if (debug) {
				fprintf(ddt,"CNAME - needs more processing\n");
			}
#endif
			if (!qp->q_cmsglen) {
				qp->q_cmsg = qp->q_msg;
				qp->q_cmsglen = qp->q_msglen;
			}
		}
	}
	/*
	 * Add the info recived in the responce to the Data Base
	 */
	c = ancount + nscount + arcount;
	nspp = nsp;
	for (i = 0; i < c; i++) {
		if (cp >= msg + msglen) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt, "FORMERR ns_resp() message bad count?\n");
#endif
			hp->rcode = FORMERR;
			goto retmsg;
		}
		if ((n = doupdate(msg, msglen, cp, 0,
		    !ancount && i < nscount)) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR ns_resp() doupdate failed\n");
#endif
			hp->rcode = FORMERR;
			goto retmsg;
		}
		cp += n;
	}
	if (cp > msg + msglen) {
#ifdef DEBUG
		if (debug)
		    fprintf(ddt,"FORMERR ns_resp()  packet size err %d, %d\n",
		       cp-msg, msglen);
#endif
		hp->rcode = FORMERR;
		goto retmsg;
	}
	if ((qtype == C_ANY) && ancount)
		goto retmsg;
	if ((!cname && !qp->q_cmsglen) && (ancount || nscount == 0))
		goto retmsg;

	/*
	 * All messages in here need further processing.  i.e. they
	 * are either CNAMEs or we got referred again.
	 */
	count = 0;
	founddata = 0;
	dname = name;
	if ((newmsg = malloc(BUFSIZ)) == NULL) {
#if DEBUG
		if (debug)
			fprintf(ddt,"ns_resp: malloc error\n");
#endif
		syslog(LOG_ERR, "ns_resp: Out Of Memory");
		hp->rcode = SERVFAIL;
		goto retmsg;
	}
	buflen = BUFSIZ;
	if ((!cname && qp->q_cmsglen) && ancount) {
#if DEBUG
		if (debug) {
			fprintf(ddt,"Cname second pass\n");
		}
#endif
		newmsglen = qp->q_cmsglen;
		bcopy(qp->q_cmsg, newmsg, newmsglen);
	} else {
		newmsglen = msglen;
		bcopy(msg, newmsg, newmsglen);
	}
	buflen = buflen - newmsglen;
	hp = (HEADER *) newmsg;
	dnptrs[0] = newmsg;
	dnptrs[1] = NULL;
	cp = newmsg + sizeof(HEADER);
	if (cname)
		cp += dn_skip(cp) + QFIXEDSZ;
	if ((n = dn_expand(newmsg, newmsg + newmsglen,
		cp, dname, sizeof(name))) < 0) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"dn_expand failed\n" );
#endif
		hp->rcode = SERVFAIL;
		free(newmsg);
		goto retmsg;
	}
	cp = newmsg + sizeof(HEADER);
	if (cname)
		cp += dn_skip(cp);
	else
		cp += n;
	cp += QFIXEDSZ;

again:
	htp = hashtab;		/* lookup relative to root */
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"ns_resp() nlookup(%s)\n",dname);
#endif
	np = nlookup(dname, &htp, &fname, 0);
	if (np == (struct namebuf *)NULL)
		fname = "";
	if (fname != dname)
		goto findns;
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"found '%s'\n", dname);
#endif
	pdp = NULL;
	dp = np->n_data;
	/* look for the data */
	while (dp != NULL) {
		if (!wanted(dp, qclass, qtype)) {
			pdp = dp;
			dp = dp->d_next;
			continue;
		}
		if ((n = make_rr(dname, dp, cp, buflen, 1)) < 0) {
			if (n == -1) {
				hp->tc = 1;
				break;
			}
			/* delete old cache entry */
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"deleting cache entry\n");
#endif
			rminv(dp);
			tmp = dp->d_next;
			free((char *)dp);
			dp = tmp;
			if (pdp == NULL)
				np->n_data = dp;
			else
				pdp->d_next = dp;
			continue;
		}
		cp += n;
		buflen -= n;
		count++;
		if (dp->d_zone)
			hp->aa = 1;
		if (dp->d_type == T_CNAME) {
			if (type == T_ANY)
				break;
			dname = dp->d_data;
			goto again;
		}
		founddata++;
		pdp = dp;
		dp = dp->d_next;
	}
#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"count = %d, founddata = %d\n", count, founddata);
#endif
	if (count)
		hp->ancount = htons((u_short)count);

findns:

	/*
 	 * Look for name servers to refer to and fill in the authority
 	 * section or record the address for forwarding the query
 	 * (recursion desired).
 	 */
	for (count = 0;; np = np->n_parent) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt, "fname = '%s'\n", fname);
#endif
		if (*fname == '\0') {
			for (np = hashtab->h_tab[0]; np != NULL;
			     np = np->n_next)
				if (np->n_dname[0] == '\0')
					goto foundns;
#ifdef DEBUG
			if (debug)
				fprintf(ddt, "No root nameserver?\n");
#endif
			syslog(LOG_ERR, "No root Nameserver\n");
			hp->rcode = SERVFAIL;
			break;
		}
foundns:
		nspp = nsp;
		pdp = NULL;
		dp = np->n_data;
		while (dp != NULL) {
			if (!match(dp, qclass, T_NS)) {
				pdp = dp;
				dp = dp->d_next;
				continue;
			}
			if (!founddata) {
				if (nspp < &nsp[MAXNS-1])
					*nspp++ = dp;
				pdp = dp;
				dp = dp->d_next;
				continue;
			}
			if ((n = make_rr(fname, dp, cp, buflen, 1)) < 0) {
				if (n == -1) {
					hp->tc = 1;
					break;
				}
				/* delete old cache entry */
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"deleting cache entry\n");
#endif
				rminv(dp);
				tmp = dp->d_next;		
				free((char *)dp);
				dp = tmp;
				if (pdp == NULL)
					np->n_data = dp;
				else
					pdp->d_next = dp;
				continue;
			}
			cp += n;
			buflen -= n;
			count++;
			pdp = dp;
			dp = dp->d_next;
		}
		if ((*fname == '\0') || (count > 0))
			break;
		if (nspp != nsp)
			break;
		if ((fname = index(fname, '.')) == NULL)
			fname = "";
		else
			fname++;
	}

	if (count && founddata) {
		hp->nscount = htons((u_short)count);
		cp += doaddinfo(hp, cp, buflen);
		buflen = cp - newmsg;
		msg = newmsg;
		msglen = buflen;
		hp = (HEADER *) msg;
		goto retmsg;
	}

	*nspp = NULL;
	if (cname) {
		newmsglen = res_mkquery(QUERY, dname, C_ANY, T_A, (char *)NULL,
					0, NULL, newmsg, BUFSIZ);
		qp->q_msglen = newmsglen;
		hp->id = qp->q_nsid;
	} else {
		hp->ancount = 0;
		hp->nscount = 0;
		hp->arcount = 0;
		hp->qr = 0;
	}
	qp->q_naddr = 0;
	qp->q_curaddr = 0;
	n = nslookup(nsp, qp);

#ifdef DEBUG
		if (debug > 7) {
			int kjd;

			fprintf(ddt,"n = %d\n",n);
			for (kjd = 0; kjd < qp->q_naddr; kjd++ )
				fprintf(ddt,"list %d-> %s (%d)\n",  kjd,
					inet_ntoa(qp->q_addr[kjd].sin_addr),
					ntohs(qp->q_addr[kjd].sin_port));
		}
#endif DEBUG
	qp->q_msg = newmsg;
 	if (qp->q_cname++ == MAXCNAMES) {
 		hp->id = qp->q_id;
 		hp->rd = 1;
 		hp->ra = 1;		
		if (qp->q_stream != QSTREAM_NULL) {
			(void) writemsg(qp->q_stream->s_rfd, newmsg, newmsglen);
			qp->q_stream->s_time = tt.tv_sec;
			qp->q_stream->s_refcnt--;
		} else {
			if (sendto(ds, newmsg, newmsglen, 0, &qp->q_from,
				sizeof(qp->q_from)) < 0) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"sendto failed\n");
#endif
			}
 		}
	 	qremove(qp);
 		return;
 	}
#ifdef DEBUG
 	if (debug)
 		fprintf(ddt,"q_cname = %d\n",qp->q_cname);
#endif
	unsched(qp);
	schedretry(qp, (time_t)RETRYTIME);

#ifdef DEBUG
	if (debug >= 3)
		fp_query(qp->q_msg, ddt);
	if (debug)
		fprintf(ddt,"try -> %s (%d)\n",
			inet_ntoa(qp->q_addr[0].sin_addr),
			ntohs(qp->q_addr[0].sin_port));
#endif
	if (sendto(ds, qp->q_msg, qp->q_msglen, 0,
		&qp->q_addr[0], sizeof(qp->q_addr[0])) < 0) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt, "error returning msg errno=%d\n",errno);
#endif
	}
	return;

retmsg:
	/*
	 * Pass answer back to original requestor.
	 */
	hp->id = qp->q_id;
	hp->rd = 1;		/* restore Recursion Desired bit */
	hp->ra = 1;		/* Recursion is Available */
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"respond -> %s (%d)\n",
			inet_ntoa(qp->q_from.sin_addr),
			ntohs(qp->q_from.sin_port));
	if (debug >= 10)
		fp_query(msg, ddt);
#endif
	if (qp->q_stream == QSTREAM_NULL) {
		if (sendto(ds, msg, msglen, 0,
		    &qp->q_from, sizeof(qp->q_from)) < 0) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"sendto failed\n");
#endif
		}

	} else {
		(void) writemsg(qp->q_stream->s_rfd, msg, msglen);
		qp->q_stream->s_time = tt.tv_sec;
		qp->q_stream->s_refcnt--;
	}
	if (msg == newmsg)
		(void) free(newmsg);
	/*
	 * Remove from table
	 */
	qremove(qp);
}

/*
 * Decode the resource record 'rrp' and update the database.
 * If savens is true, record pointer for forwarding queries a second time.
 */
doupdate(msg, msglen, rrp, zone, savens)
	char *msg ;
	char *rrp;
	int  msglen, zone, savens;
{
	register char *cp;
	register int n;
	int class, type, dlen, n1;
	u_long ttl;
	struct databuf *dp;
	char dname[MAXDNAME];
	char data[BUFSIZ], *cp1;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"doupdate(%d, %d)\n", zone, savens);
#endif

	cp = rrp;
	if ((n = dn_expand(msg, msg + msglen, cp, dname, sizeof(dname))) < 0)
		return (-1);
	cp += n;
	type = getshort(cp);
	cp += sizeof(u_short);
	class = getshort(cp);
	cp += sizeof(u_short);
	ttl = getlong(cp);
	cp += sizeof(u_long);
	dlen = getshort(cp);
	cp += sizeof(u_short);
	if (zone == 0) {
		if (ttl == 0)
			ttl = 5 * 60;
		ttl += (u_long) tt.tv_sec;
	}
	/*
	 * Convert the resource record data into the internal
	 * database format.
	 */
	switch (type) {
	case T_A:
	case T_HINFO:
	case T_UINFO:
	case T_UID:
	case T_GID:
		cp1 = cp;
		n = dlen;
		cp += n;
		break;

	case T_CNAME:
	case T_MB:
	case T_MG:
	case T_MR:
	case T_NS:
	case T_PTR:
		if ((n = dn_expand(msg, msg + msglen, cp, data,
		   sizeof(data))) < 0)
			return (-1);
		cp += n;
		cp1 = data;
		n = strlen(data) + 1;
		break;

	case T_MINFO:
	case T_SOA:
		if ((n = dn_expand(msg, msg + msglen, cp, data,
		    sizeof(data))) < 0)
			return (-1);
		cp += n;
		cp1 = data + (n = strlen(data) + 1);
		n1 = sizeof(data) - n;
		if (type == T_SOA)
			n1 -= 5 * sizeof(u_long);
		if ((n = dn_expand(msg, msg + msglen, cp, cp1, n1)) < 0)
			return (-1);
		cp += n;
		cp1 += strlen(cp1) + 1;
		if (type == T_SOA) {
			bcopy(cp, cp1, n = 5 * sizeof(u_long));
			cp += n;
			cp1 += n;
		}
		n = cp1 - data;
		cp1 = data;
		break;

  	case T_MX:
 		/* grab preference */
 		bcopy(cp,data,sizeof(u_short));
 		cp1 = data + sizeof(u_short);
 		cp += sizeof(u_short);

 		/* get name */
  		if ((n = dn_expand(msg, msg + msglen, cp, cp1,
		    sizeof(data)-sizeof(u_short))) < 0)
  			return(-1);
  		cp += n;

 		/* compute end of data */
 		cp1 += strlen(cp1) + 1;
 		/* compute size of data */
  		n = cp1 - data;
  		cp1 = data;
  		break;

	default:
#ifdef DEBUG
		if (debug >= 3)
			fprintf(ddt,"unknown type %d\n", type);
#endif
		return ((cp - rrp) + dlen);
	}
	dp = savedata(class, type, ttl, cp1, n);
	dp->d_zone = zone;
	if ((n = db_update(dname, dp, dp, DB_NODATA)) < 0) {
#ifdef DEBUG
		if (debug && (n != DATAEXISTS))
			fprintf(ddt,"update failed (%d)\n", n);
		else if (debug >= 3)
			fprintf(ddt,"update failed (DATAEXISTS)\n");
#endif
		(void) free((char *)dp);
	} else if (savens && type == T_NS && nspp < &nsp[MAXNS-1])
		*nspp++ = dp;
	return (cp - rrp);
}
