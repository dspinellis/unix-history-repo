#ifndef lint
static char sccsid[] = "@(#)ns_req.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <syslog.h>
#include <sys/file.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

#define NADDRECS	20

struct addinfo {
	char	*a_dname;		/* domain name */
	u_short	a_class;		/* class for address */
};

struct	addinfo addinfo[NADDRECS];	/* additional info records */
int	addcount;			/* number of names in addinfo */

char	*dnptrs[20];		/* ptrs to dnames in message for dn_comp */

extern char *malloc();
extern int errno;
/*
 * Process request using database; assemble and send response.
 */
ns_req(msg, msglen, buflen, qsp, from)
	char *msg;
	int msglen, buflen;
	struct qstream *qsp;
	struct sockaddr_in *from;
{
	register HEADER *hp;
	register char *cp;
	register struct namebuf *np;
	register struct databuf *dp;
	struct databuf *tmp;
	struct databuf *pdp;
	struct hashbuf *htp;
	struct zoneinfo *zp;
	char *fname;
	char dnbuf[MAXDNAME], *dname;
	char **dpp;
	char *dnp;
	char *newmsg;
	int n, class, type, count, foundname, founddata, cname = 0;
	int newmsglen;
	u_short id;
	struct databuf *nsp[MAXNS], **nspp;
	struct qinfo *qp;
	time_t curtime;
	extern struct qinfo *qhead;
	extern int nsid;

#ifdef DEBUG
	if (debug > 3) {
		fprintf(ddt,"ns_req()\n");
		fp_query(msg, ddt);
	}
#endif

	hp = (HEADER *) msg;
	if (hp->qr) {
		ns_resp(msg, msglen);
		return;
	}

	cp = msg + sizeof(HEADER);
	hp->rcode = NOERROR;
	dpp = dnptrs;
	*dpp++ = msg;
	addcount = 0;


	switch (hp->opcode) {
	case QUERY:
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
			break;
		}
		/*
		 * Get domain name, class, and type.
		 */
		if ((*cp & INDIR_MASK) == 0)
			*dpp++ = cp;	/* remember name for compression */
		*dpp = NULL;
		if ((n = dn_expand(msg, msg + msglen, cp, dnbuf,
		    sizeof(dnbuf))) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR Query expand name failed\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		cp += n;
		type = getshort(cp);
		cp += sizeof(u_short);
		class = getshort(cp);
		cp += sizeof(u_short);
		if (cp < msg + msglen) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR Query message length off\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
#ifdef DEBUG
		if (cp > msg + msglen)
			if (debug > 5)
			    fprintf(ddt,"message length > recived message\n");
#endif
#ifdef notdef
		if (!check_class(quest.qtype->class)) {
			if (debug)
			    fprintf(ddt,"FORMERR Query class is wrong\n");
			hp->rcode = FORMERR;
			break;
		}
		if (!check_type(quest.qtype->type)) {
			if (debug)
			   fprintf(ddt,"proc_query FORMERR Query type wrong\n");
			hp->rcode = FORMERR;
			break;
		}
#endif
		/*
		 * Process query.
		 */
		if (type == T_AXFR) {
			/* refuse request if not a TCP connection */
			if (qsp == QSTREAM_NULL) {
				hp->rcode = REFUSED;
				break;
			}
			/* don't compress names */
			dnptrs[0] = NULL;
		}
		buflen -= msglen;
		count = 0;
		foundname = 0;
		founddata = 0;
		dname = dnbuf;
	again:
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"nlookup(%s)\n", dname);
#endif
		htp = hashtab;		/* lookup relative to root */
		np = nlookup(dname, &htp, &fname, 0);
		/*
		 * if nlookup failed to find address then
		 *  see if there are any '.''s in the name
		 * if not then add local domain name to the
		 * name and try again.
		 */
		if (np == NULL) {
			fname = "";
			dnp = &dname[strlen(dname)];
			if ( index(dname, '.') == NULL)
			for (zp = zones; zp < &zones[nzones]; zp++) {
				if ( zp->z_type == Z_DOMAIN){
#ifdef DEBUG
					if (debug >= 5)
						fprintf(ddt,"domain = %s\n",
						    zp->z_origin);
#endif
					(void) strcat(dname,".");
					(void) strcat(dname,zp->z_origin);
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,"nlookup(%s)\n", dname);
#endif
					np = nlookup(dname, &htp, &fname, 0);
					if (np != NULL)
						break;
					fname = "";
					*dnp = '\0';
				}
			}
	        }
		if (fname != dname) {
#ifdef DEBUG
			if (debug && cname) {
			    if (founddata)
			        fprintf(ddt,"CNAME with data\n");
			    else
			        fprintf(ddt,"CNAME without data %s\n", dname);
			}
#endif
			goto findns;
		}
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"found '%s'\n", dname);
#endif
		foundname++;
		pdp = NULL;
		dp = np->n_data;
		/* look for the data */
		while (dp != NULL) {
			if (!wanted(dp, class, type)) {
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
				if (debug)
					fprintf(ddt,"deleting cache entry\n");
#endif
				rminv(dp);
				tmp = dp->d_next;		
				(void) free((char *)dp);
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
				cname++;
				dname = dp->d_data;
				goto again;
			}
			founddata++;
			pdp = dp;
			dp = dp->d_next;
		}
#ifdef DEBUG
		if (debug >= 5) {
		    fprintf(ddt,"foundname = %d count = %d ", foundname, count);
		    fprintf(ddt,"founddata = %d cname = %d\n",founddata, cname);
		}
#endif
		if (count) {
			hp->ancount = htons((u_short)count);
			if (type == T_AXFR && founddata) {
#ifdef DEBUG
				if (debug >= 5)
					fprintf(ddt,"doing axfr()\n");
#endif
				/*
				 * child does the work while
				 * the parent continues
				 */
				if (fork() == 0) {
				    register FILE *rfp;
				    int fdstat;

				    rfp = fdopen(qsp->s_rfd, "w");
				    setproctitle("zone XFR to", qsp->s_rfd);
				    fdstat = fcntl(qsp->s_rfd, F_GETFL, 0);
				    if (fdstat != -1)
					(void) fcntl(qsp->s_rfd, F_SETFL,
					    fdstat & ~FNDELAY);
				    fwritemsg(rfp, msg, cp - msg);
				    doaxfr(np, rfp, 1);
				    fwritemsg(rfp, msg, cp - msg);
				    (void) fflush(rfp);
				    exit(0);
				}
				qsp->s_time = tt.tv_sec;
				qsp->s_refcnt--;
				return;
			}
			if (hp->aa)
				break;
		}
	findns:
		/*
		 * Look for name servers to refer to and
		 * fill in the authority section or record the address
		 * for forwarding the query (recursion desired).
		 */
		for (count = 0; ; np = np->n_parent) {
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"fname = '%s'\n", fname);
#endif
			if (*fname == '\0') {
				for (np = hashtab->h_tab[0]; np != NULL;
				    np = np->n_next)
					if ( np->n_dname[0] == '\0')
						goto foundns;
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"No root nameserver?\n");
#endif
				syslog(LOG_ERR,"No root Nameserver\n");
				hp->rcode = SERVFAIL;
				break;
			}
		foundns:
			nspp = nsp;	/* record ns records if forwarding */
			pdp = NULL;
			dp = np->n_data;
			curtime =(u_long) tt.tv_sec;
			while (dp != NULL) {
				if (dp->d_zone && match(dp, class, T_SOA)) {
					if (!foundname)
						hp->rcode = NXDOMAIN;
					hp->aa = 1;
					goto finish;
				}
				if (!match(dp, class, T_NS)) {
					pdp = dp;
					dp = dp->d_next;
					continue;
				}
				if ((dp->d_zone == 0) &&
				    (dp->d_ttl < curtime)) {
					/* delete old cache entry */
#ifdef DEBUG
					if (debug)
					   fprintf(ddt,"deleting cache entry\n");
#endif
					rminv(dp);
					tmp = dp->d_next;		
					(void) free((char *)dp);
					dp = tmp;
					if (pdp == NULL)
						np->n_data = dp;
					else
						pdp->d_next = dp;
					continue;
				}
				if (hp->rd && !founddata) {
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
					(void) free((char *)dp);
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
			if (count && founddata) {
				hp->nscount = htons((u_short)count);
				break;
			}
			if (nspp != nsp) {
			    *nspp = NULL;
			    if (cname) {
				id = hp->id;	
			        for (qp = qhead; qp!=QINFO_NULL;
				   qp = qp->q_link) {
				    if (qp->q_id == id &&
					qp->q_cmsglen == msglen &&
		    		        bcmp((char *)qp->q_cmsg+2,
					    msg+2, msglen-2) == 0)
					    return;
			        }
				/* build new qinfo struct */
				qp = qnew();
				qp->q_naddr = 0;
				if (nslookup(nsp, qp) == 0) {
#ifdef DEBUG
				    if (debug >= 5)
				       fprintf(ddt,"none found in nsp\n");
#endif
				     qfree(qp);
			             hp->rcode = SERVFAIL;
				     break;
				}
				qp->q_cname++;
				qp->q_stream = qsp;
				qp->q_curaddr = 0;
				qp->q_id = id;
				qp->q_from = *from;
				if ((qp->q_cmsg = malloc((unsigned)msglen))
					== NULL) {
#if DEBUG
			            if (debug)
			                fprintf(ddt,"ns_req: malloc fail\n");
#endif
			            syslog(LOG_ERR, "ns_req: Out Of Memory");
			            hp->rcode = SERVFAIL;
				    break;
				}
				bcopy(msg, qp->q_cmsg, qp->q_cmsglen = msglen);
			        if ((newmsg = malloc(BUFSIZ)) == NULL) {
#if DEBUG
			            if (debug)
			                fprintf(ddt,"ns_req: malloc error\n");
#endif
			            syslog(LOG_ERR, "ns_req: Out Of Memory");
			            hp->rcode = SERVFAIL;
			            goto finish;
		                }
			        buflen = BUFSIZ;
			        dnptrs[0] = newmsg;
			        dnptrs[1] = NULL;
				newmsglen = res_mkquery(QUERY, dname, class,
				    type, (char *)NULL, 0, NULL,
				    newmsg, buflen);
				qp->q_msg = newmsg;
				qp->q_msglen = newmsglen;
				hp = (HEADER *) newmsg;
				hp->id = qp->q_nsid = htons((u_short)++nsid);
				hp->ancount = 0;
				hp->nscount = 0;
				hp->arcount = 0;
				schedretry(qp, (time_t)RETRYTIME);

#ifdef DEBUG
				if (debug)
				    fprintf(ddt,"forw -> %s (%d)\n",
				        inet_ntoa(qp->q_addr[0].sin_addr),
			                ntohs(qp->q_addr[0].sin_port));
				if ( debug >= 10)
				    fp_query(newmsg, ddt);
#endif
				if (sendto(ds, newmsg, newmsglen, 0,
				    &qp->q_addr[0], sizeof(qp->q_addr[0])) < 0){
#ifdef DEBUG
				    if (debug)
				        fprintf(ddt,"sendto error \n");
#endif
				}
				return;
			    }
			    if ((n = ns_forw(nsp, msg, msglen, from, qsp)) == 0)
					return;
				if (n == -2) {
					hp->rcode = SERVFAIL;
					break;
				}
			}
			if (*fname == '\0')
				break;
			if ((fname = index(fname, '.')) == NULL)
				fname = "";
			else
				fname++;
		}
		break;

	case IQUERY: {
		register struct invbuf *ip;
		register int i;
		int dlen, alen;
		char anbuf[PACKETSZ], *data;

		if (ntohs(hp->ancount) != 1 ||
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
			break;
		}
		/*
		 * Skip domain name, get class, and type.
		 */
		if ((n = dn_skip(cp)) < 0) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR IQuery packet name problem\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		cp += n;
		type = getshort(cp);
		cp += sizeof(u_short);
		class = getshort(cp);
		cp += sizeof(u_short) + sizeof(u_long);
		dlen = getshort(cp);
		cp += sizeof(u_short) + dlen;
		if (cp != msg + msglen) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR IQuery message length off\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		/* not all inverse queries are handled. */
		switch (type) {
		case T_A:
		case T_UID:
		case T_GID:
			break;

		default:
			hp->rcode = REFUSED;
			goto finish;
		}
		fname = msg + sizeof(HEADER);
		bcopy(fname, anbuf, alen = cp - fname);
		data = anbuf + alen - dlen;
		cp = fname;
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
				buflen -= QFIXEDSZ;
				if ((n =
				   dn_comp(dnbuf, cp, buflen, (char **)NULL,
				      (char **)NULL)) < 0)
				{
					hp->tc = 1;
					goto finish;
				}
				cp += n;
				putshort((u_short)dp->d_type, cp);
				cp += sizeof(u_short);
				putshort((u_short)dp->d_class, cp);
				cp += sizeof(u_short);
				buflen -= n;
				count++;
			}
		    }
		}
		hp->qdcount = htons((u_short)count);
		if (alen > buflen) {
			hp->tc = 1;
			break;
		}
		bcopy(anbuf, cp, alen);
		cp += alen;
		break;
	    }

#ifdef notdef
	case UPDATEA:
		if ((n =
		   dn_expand(msg, msg + msglen, cp, dnbuf, sizeof(dnbuf))) < 0)
		{
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR UpdateA expand name failed\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		cp += n;
		type = getshort(cp);
		cp += sizeof(u_short);
		class = getshort(cp);
		cp += sizeof(u_short);
		ttl = getlong(cp);
		cp += sizeof(u_long);
		n = getshort(cp);
		cp += sizeof(u_short);
		if (cp + n != msg + msglen) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR UpdateA expand name failed\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		dp = savedata(class, type, ttl, cp, n);
		dp->d_zone = findzone(dnbuf, class);
		if ((n = updatedb(dnbuf, NULL, dp, DB_NODATA)) != NOERROR) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"update failed\n");
#endif
			hp->rcode = n;
		}
		hp->rcode = NOERROR;
		break;

	case UPDATED:
		if ((n =
		    dn_expand(msg, msg + msglen, cp, dnbuf, sizeof(dnbuf))) < 0)
		{
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR UpdateD expand name failed\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		cp += n;
		rrec.r_type = getshort(cp);
		cp += sizeof(u_short);
		rrec.r_class = getshort(cp);
		cp += sizeof(u_short) + sizeof(u_long);
		rrec.r_size = getshort(cp);
		rrec.r_data = cp += sizeof(u_short);
		if (cp + rrec.r_size != msg + msglen) {
#ifdef DEBUG
			if (debug)
			    fprintf(ddt,"FORMERR UpdateD message length off\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		if (updatedb(dnbuf, &rrec, NULL, DB_DELETE) < 0) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"update failed\n");
#endif
		}
		hp->rcode = NOERROR;
		break;

	case UPDATEM:
		if ((n = dn_expand(msg, msg + msglen, cp, addbuf,
		    sizeof(addbuf))) < 0) {
#ifdef DEBUG
			if (debug)
			   fprintf(ddt,"FORMERR UpdateM expand name 1 failed\n");
#endif
			hp->rcode = FORMERR;
			break;
		}
		cp += n;
		rrec.r_type = getshort(cp);
		cp += sizeof(u_short);
		rrec.r_class = getshort(cp);
		cp += sizeof(u_short) + sizeof(u_long);
		rrec.r_size = getshort(cp);
		rrec.r_data = cp += sizeof(u_short);
		cp += rrec.r_size;
		if ((n =
		   dn_expand(msg, msg + msglen, cp, dnbuf, sizeof(dnbuf))) < 0)
	 	{
#ifdef DEBUG
		   if (debug)
		      fprintf(ddt,"FORMERR UpdateM expand name 2 failed\n");
#endif
		   hp->rcode = FORMERR;
		   break;
		}
		if (cistrcmp(dnbuf, addbuf) != 0) {
#ifdef DEBUG
		   if (debug)
		      fprintf(ddt,"FORMERR UpdateM string compair failed\n");
#endif
		   hp->rcode = FORMERR;
		   break;
		}
		cp += n;
		type = getshort(cp);
		cp += sizeof(u_short);
		class = getshort(cp);
		cp += sizeof(u_short);
		ttl = getlong(cp);
		cp += sizeof(u_long);
		n = getshort(cp);
		cp += sizeof(u_short);
		if (cp + n != msg + msglen) {
#ifdef DEBUG
		   if (debug)
		   	fprintf(ddt,"FORMERR UpdateM message length off\n");
#endif
		   hp->rcode = FORMERR;
		   break;
		}
		dp = savedata(class, type, ttl, cp, n);
		dp->d_zone = findzone(dnbuf, class);
		if ((n =
		   updatedb(dnbuf, &rrec, dp, DB_MEXIST|DB_DELETE)) != NOERROR)
		{
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"update failed\n");
#endif
			hp->rcode = n;
		}
		hp->rcode = NOERROR;
		break;
#endif

	case ZONEREF:
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"Refresh Zone\n");
#endif

	default:
		hp->rcode = NOTIMP;
		hp->qdcount = 0;
		hp->ancount = 0;
		hp->nscount = 0;
		hp->arcount = 0;
	}
finish:
	hp->qr = 1;		/* set Response flag */
	hp->ra = 1;		/* Recursion is Available */
	if (addcount)
		cp += doaddinfo(hp, cp, buflen - (cp - msg));
#ifdef DEBUG
	if (debug >= 10)
		fp_query(msg, ddt);
#endif
	if (qsp == QSTREAM_NULL) {
		if (sendto(ds, msg, cp-msg, 0, from, sizeof(*from)) < 0) {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"error returning msg\n");
#endif
		}
	} else {
		(void) writemsg(qsp->s_rfd, msg, cp - msg);
		qsp->s_time = tt.tv_sec;
		qsp->s_refcnt--;
	}
}

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
		/*
		syslog(LOG_ERR, "fwritemsg: write failed: %m");
		*/
	}
	return;
}

writemsg(rfd, msg, msglen)
	int rfd;
	char *msg;
	int msglen;
{
	struct iovec iov[2];
	u_short len = htons((u_short)msglen);

	iov[0].iov_base = (caddr_t)&len;
	iov[0].iov_len = sizeof(len);
	iov[1].iov_base = msg;
	iov[1].iov_len = msglen;
	if (writev(rfd, iov, 2) != sizeof(len) + msglen) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"write failed %d\n", errno);
#endif
		/*
		syslog(LOG_ERR, "writemsg: write failed: %m");
		*/
		return (-1);
	}
	return (0);
}

/*
 * Copy databuf into a resource record for replies.
 * Return size of RR if OK, -1 if buffer is full and
 * -2 if its an outdated cache entry.
 */
make_rr(name, dp, buf, buflen, doadd)
	char *name;
	register struct databuf *dp;
	char *buf;
	int buflen, doadd;
{
	register char *cp;
	register struct addinfo *ap;
	char *cp1, *sp;
	register long n;
	u_long ttl;
	char **edp = dnptrs + sizeof(dnptrs)/sizeof(dnptrs[0]);

#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"make_rr(%s, %x, %x, %d, %d) %d\n", name, dp, buf,
			buflen, doadd, dp->d_size);
#endif

	/* check for outdated RR before updating dnptrs by dn_comp() */
	if (dp->d_zone == 0) {
		ttl = dp->d_ttl - (u_long) tt.tv_sec;
		if (dp->d_ttl < (u_long)tt.tv_sec)
			return (-2);
	} else {
		ttl = zones[dp->d_zone].z_minimum;
		if (dp->d_ttl > ttl)
			ttl = dp->d_ttl;
	}

	buflen -= RRFIXEDSZ;
	if ((n = dn_comp(name, buf, buflen, (char **)dnptrs, (char **)edp)) < 0)
		return (-1);
	cp = buf + n;
	buflen -= n;
	putshort((u_short)dp->d_type, cp);
	cp += sizeof(u_short);
	putshort((u_short)dp->d_class, cp);
	cp += sizeof(u_short);
	putlong(ttl, cp);
	cp += sizeof(u_long);
	sp = cp;
	cp += sizeof(u_short);
	switch (dp->d_type) {
	case T_CNAME:
	case T_MG:
	case T_MR:
	case T_PTR:
		if ((n = dn_comp(dp->d_data, cp, buflen, (char **)dnptrs,
		   (char **)edp)) < 0)
			return (-1);
		putshort((u_short)n, sp);
		cp += n;
		break;

	case T_MB:
	case T_NS:
		/* Store domain name in answer */
		if ((n = dn_comp(dp->d_data, cp, buflen, (char **)dnptrs,
		   (char **)edp)) < 0)
			return (-1);
		putshort((u_short)n, sp);
		cp += n;
		if (!doadd)
			break;
		for (ap = addinfo, n = addcount; --n >= 0; ap++)
			if (cistrcmp(ap->a_dname, dp->d_data) == 0)
				goto found;
		/* add domain name to additional section */
		if (addcount >= NADDRECS)
			break;
		addcount++;
		ap->a_dname = dp->d_data;
		ap->a_class = dp->d_class;
	found:
		break;

	case T_SOA:
	case T_MINFO:
		cp1 = dp->d_data;
		if ((n = dn_comp(cp1, cp, buflen, (char **)dnptrs,
		    (char **)edp)) < 0)
			return (-1);
		cp += n;
		buflen -= dp->d_type == T_MINFO ? n : n + 5 * sizeof(u_long);
		cp1 += strlen(cp1) + 1;
		if ((n = dn_comp(cp1, cp, buflen, (char **)dnptrs,
		    (char **)edp)) < 0)
			return (-1);
		cp += n;
		if (dp->d_type == T_SOA) {
			cp1 += strlen(cp1) + 1;
			bcopy(cp1, cp,
			   (int)(n = dp->d_size - (cp1 - dp->d_data)));
			cp += n;
		}
		putshort((u_short)(cp - sp) - sizeof(u_short), sp);
		break;

	case T_MX:
		/* cp1 == our data/ cp == data of RR */
		cp1 = dp->d_data;

 		/* copy preference */
 		bcopy(cp1,cp,sizeof(u_short));
 		cp += sizeof(u_short);
 		cp1 += sizeof(u_short);
 		buflen -= sizeof(u_short);

  		if ((n = dn_comp(cp1, cp, buflen, (char **)dnptrs,
		    (char **)edp)) < 0)
  			return(-1);
  		cp += n;

  		/* save data length */
  		putshort((u_short)(cp-sp)-sizeof(u_short),sp);
  		break;

	default:
		if (dp->d_size > buflen)
			return (-1);
		bcopy(dp->d_data, cp, dp->d_size);
		putshort((u_short)dp->d_size, sp);
		cp += dp->d_size;
	}
	return (cp - buf);
}

/*
 * Lookup addresses for names in addinfo and put into the message's
 * additional section.
 */
doaddinfo(hp, msg, msglen)
	HEADER *hp;
	char *msg;
	int msglen;
{
	register char *cp;
	register struct namebuf *np;
	register struct databuf *dp;
	register struct databuf *tmp;
	struct hashbuf *htp;
	struct databuf *pdp;
	struct addinfo *ap;
	char *fname;
	int n, count;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"doaddinfo() addcount = %d\n", addcount);
#endif

	count = 0;
	cp = msg;
	for (ap = addinfo; --addcount >= 0; ap++) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"do additional '%s'\n", ap->a_dname);
#endif
		htp = hashtab;		/* lookup relative to root */
		np = nlookup(ap->a_dname, &htp, &fname, 0);
		if (np == NULL || fname != ap->a_dname)
			continue;
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"found it\n");
#endif
		pdp = NULL;
		dp = np->n_data;
		/* look for the data */
		while (dp != NULL) {
			if (!match(dp, (int)ap->a_class, T_A)) {
				pdp = dp;
				dp = dp->d_next;
				continue;
			}
			if ((n = make_rr(ap->a_dname, dp, cp, msglen, 0)) < 0)
			{
				if (n == -1)
					break;
				/* delete old cache entry */
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"deleting cache entry\n");
#endif
				rminv(dp);
				tmp = dp->d_next;		
				(void) free((char *)dp);
				dp = tmp;
				if (pdp == NULL)
					np->n_data = dp;
				else
					pdp->d_next = dp;
				continue;
			}
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"n = %d\n", n);
#endif
			cp += n;
			msglen -= n;
			count++;
			pdp = dp;
			dp = dp->d_next;
		}
	}
	hp->arcount = htons((u_short)count);
	return (cp - msg);
}

/*
 * Do we want this data record based on the class and type?
 */
wanted(dp, class, type)
	struct databuf *dp;
	int class, type;
{

#ifdef DEBUG
	if (debug > 3)
		fprintf(ddt,"wanted(%x, %d, %d) %d, %d\n", dp, class, type,
			dp->d_class, dp->d_type);
#endif

	if (dp->d_class != class && class != C_ANY)
		return (0);
	if (type == dp->d_type)
		return (1);
	switch (dp->d_type) {
	case T_ANY:
	case T_CNAME:
		return (1);
	}
	switch (type) {
	case T_ANY:
		return (1);

	case T_MAILB:
		switch (dp->d_type) {
		case T_MR:
		case T_MB:
		case T_MG:
		case T_MINFO:
			return (1);
		}
		break;

	case T_AXFR:
		if (dp->d_type == T_SOA)
			return (1);
	}
	return (0);
}

/*
 * Get the domain name of 'np' and put in 'buf'.
 */
getname(np, buf, buflen)
	struct namebuf *np;
	char *buf;
	int buflen;
{
	register char *cp;

	cp = buf;
	while (np != NULL) {
		if (cp != buf)
			*cp++ = '.';
		(void) strcpy(cp, np->n_dname);
		cp += strlen(cp);
		np = np->n_parent;
	}
	*cp = '\0';
}

/*
 * Do a zone transfer. SOA record already sent.
 */
doaxfr(np, rfp, isroot)
	register struct namebuf *np;
	FILE *rfp;
	int isroot;
{
	register struct databuf *dp;
	register int n;
	struct namebuf **npp, **nppend;
	char msg[PACKETSZ];
	HEADER *hp = (HEADER *) msg;
	char *cp;
	char dname[MAXDNAME];
	int fndns;

#ifdef DEBUG
	if (debug && isroot)
		fprintf(ddt,"doaxfr()\n");
#endif
	fndns = 0;
	hp->id = 0;
	hp->opcode = QUERY;
	hp->qr = hp->aa = hp->tc = hp->ra = hp->pr = hp->rd = 0;
	hp->rcode = NOERROR;
	hp->qdcount = 0;
	hp->ancount = htons(1);
	hp->nscount = 0;
	hp->arcount = 0;
	cp = msg + sizeof(HEADER);
	getname(np, dname, sizeof(dname));

	/* first do data records */
	for (dp = np->n_data; dp != NULL; dp = dp->d_next) {
		/* skip the root SOA record (marks end of data) */
		if (isroot) {
			if (dp->d_type == T_SOA)
				continue;
		} else if (dp->d_type == T_NS)
			fndns = 1;
		if (dp->d_zone == 0)
			continue;
		if ((n = make_rr(dname, dp, cp,
		    sizeof(msg)-sizeof(HEADER), 0)) < 0)
			continue;
		fwritemsg(rfp, msg, n + sizeof(HEADER));
	}

	/* next do subdomains */
	if (fndns || np->n_hash == NULL)
		return;
	npp = np->n_hash->h_tab;
	nppend = npp + np->n_hash->h_size;
	while (npp < nppend) {
		for (np = *npp++; np != NULL; np = np->n_next) {
			doaxfr(np, rfp, 0);
		}
	}
#ifdef DEBUG
	if (debug && isroot)
		fprintf(ddt,"exit doaxfr()\n");
#endif
}
