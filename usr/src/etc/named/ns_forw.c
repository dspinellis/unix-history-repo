#ifndef lint
static char sccsid[] = "@(#)ns_forw.c	4.3 (Berkeley) 6/4/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <syslog.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

struct	qinfo *qhead = QINFO_NULL;	/* head of allocated queries */
struct	qinfo *retryqp = QINFO_NULL;	/* list of queries to retry */

int	nsid;				/* next forwarded query id */
extern int errno;

/*
 * Forward the query to get the answer since its not in the database.
 */
ns_forw(nsp, msg, msglen, fp, qsp)
	struct databuf *nsp[];
	char *msg;
	int msglen;
	struct sockaddr_in *fp;
	struct qstream *qsp;
{
	register struct qinfo *qp;
	HEADER *hp;
	u_short id;
	extern char *calloc();
	extern char *malloc();

#ifdef DEBUG
	if (debug > 3)
		fprintf(ddt,"ns_forw()\n");
#endif

	/* Don't forward if we're already working on it. */
	hp = (HEADER *) msg;
	id = hp->id;
	hp->rd = 0;
	/* Look at them all */
	for (qp = qhead; qp!=QINFO_NULL; qp = qp->q_link) {
		if (qp->q_id == id && qp->q_msglen == msglen &&
		    bcmp((char *)qp->q_msg+2, msg+2, msglen-2) == 0)
			return (0);
	}

	qp = qnew();
	qp->q_naddr = 0;
	if (nslookup(nsp, qp) == 0) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"none found in nsp\n");
#endif
		qfree(qp);
		return (-1);
	}
	qp->q_stream = qsp;
	qp->q_curaddr = 0;
	qp->q_id = id;
	hp->id = qp->q_nsid = htons((u_short)++nsid);
	hp->ancount = 0;
	hp->nscount = 0;
	hp->arcount = 0;
	qp->q_from = *fp;
	if ((qp->q_msg = malloc((unsigned)msglen)) == NULL) {
		syslog(LOG_ERR, "forw: %m");
		exit(1);
	}
	bcopy(msg, qp->q_msg, qp->q_msglen = msglen);

	schedretry(qp, (time_t)RETRYTIME);
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"forw -> %s (%d)\n",
			inet_ntoa(qp->q_addr[0].sin_addr),
			ntohs(qp->q_addr[0].sin_port));
	if ( debug >= 10)
		fp_query(msg, ddt);
#endif
	if (sendto(ds, msg, msglen, 0, &qp->q_addr[0],
	    sizeof(qp->q_addr[0])) < 0) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"error forwarding msg\n");
#endif
	}
	return (0);
}

/*
 * Lookup the address for each nameserver in `nsp' and add it to
 * the list saved in the qinfo structure.
 */
nslookup(nsp, qp)
	struct databuf *nsp[];
	struct qinfo *qp;
{
	register struct namebuf *np;
	register struct databuf *dp, *pdp;
	register int n, i;
	struct databuf *tmp;
	struct hashbuf *htp;
	char *dname, *fname;
	int naddr, class;
	time_t curtime;

	extern short ns_port;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"nslookup(nsp=x%x,qp=x%x)\n",nsp,qp);
#endif

	naddr = n = qp->q_naddr;
	curtime = (u_long) tt.tv_sec;
	while ((dp = *nsp++) != NULL) {
		dname = dp->d_data;
		class = dp->d_class;
		htp = hashtab;		/* lookup relative to root */
		np = nlookup(dname, &htp, &fname, 0);
		if (np == NULL || fname != dname) {
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"%s: not found\n", dname);
#endif
			continue;
		}
		/* look for name server addresses */
		pdp = NULL;
		dp = np->n_data;
		while (dp != NULL) {
			if (!match(dp, class, T_A))
				goto skip;
			if ((dp->d_zone == 0) && (dp->d_ttl < curtime)) {
				/* delete old cache entry */
#ifdef DEBUG
				if (debug >= 5)
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
			/* don't put in duplicates */
			for (i = 0; i < n; i++)
				if (bcmp((char *)&qp->q_addr[i].sin_addr,
				    dp->d_data, sizeof(struct in_addr)) == 0)
					goto skip;
			if (n >= MAXNS)
				break;
			qp->q_addr[n].sin_family = AF_INET;
			qp->q_addr[n].sin_addr = *(struct in_addr *)dp->d_data;
			qp->q_addr[n].sin_port = (u_short)ns_port;
			qp->q_nretry[n] = 0;
			n++;
		skip:	pdp = dp;
			dp = dp->d_next;
		}
		if (n >= MAXNS) {
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt,"q_addr table full\n");
#endif
			break;
		}
	}
	qp->q_naddr = n;
	return (n - naddr);
}

/*
 * Arrange that forwarded query (qp) is retried after t seconds.
 */
schedretry(qp, t)
	struct qinfo *qp;
	time_t t;
{
	register struct qinfo *qp1, *qp2;

#ifdef DEBUG
	if (debug > 3) {
		fprintf(ddt,"schedretry(%#x, %d)\n", qp, t);
		if (qp->q_time)
		   fprintf(ddt,"WARNING: schedretry(%x,%d) q_time already %d\n",		    qp->q_time);
	}
#endif
	t += (u_long) tt.tv_sec;
	qp->q_time = t;

	if ((qp1 = retryqp) == NULL) {
		retryqp = qp;
		qp->q_next = NULL;
		return;
	}
	while ((qp2 = qp1->q_next) != NULL && qp2->q_time < t)
		qp1 = qp2;
	qp1->q_next = qp;
	qp->q_next = qp2;
}

/*
 * Unsched is called to remove a forwarded query entry.
 */
unsched(qp)
	struct qinfo *qp;
{
	register struct qinfo *np;

#ifdef DEBUG
	if (debug > 3) {
		fprintf(ddt,"unsched(%#x, %d )\n", qp, qp->q_id);
	}
#endif
	if( retryqp == qp )  {
		retryqp = qp->q_next;
	} else {
		for( np=retryqp; np->q_next != QINFO_NULL; np = np->q_next ) {
			if( np->q_next != qp)
				continue;
			np->q_next = qp->q_next;	/* dequeue */
			break;
		}
	}
	qp->q_next = QINFO_NULL;		/* sanity check */
	qp->q_time = 0;
}

/*
 * Retry is called to retransmit query 'qp'.
 */
retry(qp)
	register struct qinfo *qp;
{
	register int n;
	register HEADER *hp;

#ifdef DEBUG
	if (debug > 3)
		fprintf(ddt,"retry(x%x) id=%d\n", qp, ntohs(qp->q_id));
#endif

	/* try next address */
	n = qp->q_curaddr;
	++qp->q_nretry[n];
	do {
		if (++n >= qp->q_naddr)
			n = 0;
		if (qp->q_nretry[n] < MAXRETRY)
			goto found;
	} while (n != qp->q_curaddr);
	/*
	 * Give up. Can't reach destination.
	 */
#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"give up\n");
#endif
	hp = (HEADER *)qp->q_msg;
	hp->qr = 1;
	hp->rcode = SERVFAIL;
	hp->ra = 1;
#ifdef DEBUG
	if (debug >= 10)
		fp_query(qp->q_msg, ddt);
#endif
	if (sendto(ds, qp->q_msg, qp->q_msglen, 0, &qp->q_from,
	    sizeof(qp->q_from))) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"gave up retry(x%x) id=%d\n",
				qp, ntohs(qp->q_id));
#endif
	}
	qremove(qp);
	return;
found:
	qp->q_curaddr = n;
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"resend(id=%d n=%d) -> %s (%d)\n",ntohs(qp->q_id),
				n, inet_ntoa(qp->q_addr[n].sin_addr),
				ntohs(qp->q_addr[n].sin_port));
	if ( debug >= 10)
		fp_query(qp->q_msg, ddt);
#endif
	if (sendto(ds, qp->q_msg, qp->q_msglen, 0, &qp->q_addr[n],
	    sizeof(qp->q_addr[0])) < 0) {
#ifdef DEBUG
		if (debug > 3)
			fprintf(ddt,"error returning msg\n");
#endif
	}
	unsched(qp);
	schedretry(qp, (time_t)RETRYTIME);
}

qremove(qp)
register struct qinfo *qp;
{
#ifdef DEBUG
	if(debug > 3)
		fprintf(ddt,"qremove(x%x)\n", qp);
#endif
	unsched(qp);			/* get off queue first */
 	free(qp->q_msg);
 	if (qp->q_cmsg);
 		free(qp->q_cmsg);
	qfree(qp);
}

struct qinfo *
qfindid(id)
register u_short id;
{
	register struct qinfo *qp;

#ifdef DEBUG
	if(debug > 3)
		fprintf(ddt,"qfindid(%d)\n", ntohs(id));
#endif
	for (qp = qhead; qp!=QINFO_NULL; qp = qp->q_link) {
		if (qp->q_nsid == id)
			return(qp);
	}
#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"qp not found\n");
#endif
	return(NULL);
}

struct qinfo *
qnew()
{
	register struct qinfo *qp;

	if ((qp = (struct qinfo *)calloc(1, sizeof(struct qinfo))) == NULL) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"qnew: calloc error\n");
#endif
		syslog(LOG_ERR, "forw: %m");
		exit(12);
	}
#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"qnew(x%x)\n", qp);
#endif
	qp->q_link = qhead;
	qhead = qp;
	return( qp );
}

qfree(qp)
struct qinfo *qp;
{
	register struct qinfo *np;

#ifdef DEBUG
	if(debug > 3)
		fprintf(ddt,"qfree( x%x )\n", qp);
	if(debug && qp->q_next)
		fprintf(ddt,"WARNING:  qfree of linked ptr x%x\n", qp);
#endif
	if( qhead == qp )  {
		qhead = qp->q_link;
	} else {
		for( np=qhead; np->q_link != QINFO_NULL; np = np->q_link )  {
			if( np->q_link != qp )  continue;
			np->q_link = qp->q_link;	/* dequeue */
			break;
		}
	}
	(void)free((char *)qp);
}
