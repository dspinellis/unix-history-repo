/*
 *	@(#)ns.h	4.33 (Berkeley) 8/23/90
 *	$Id: ns.h,v 4.9.1.1 1993/05/02 22:43:03 vixie Rel $
 */

/*
 * ++Copyright++ 1986
 * -
 * Copyright (c) 1986 Regents of the University of California.
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

/*
 * Global definitions and variables for the name server.
 */

/*
 * Timeout time should be around 1 minute or so.  Using the
 * the current simplistic backoff strategy, the sequence
 * retrys after 4, 8, and 16 seconds.  With 3 servers, this
 * dies out in a little more than a minute.
 * (sequence RETRYBASE, 2*RETRYBASE, 4*RETRYBASE... for MAXRETRY)
 */
#define MINROOTS	2	 	/* min number of root hints */
#define NSMAX		16		/* max number of NS addrs to try */
#define RETRYBASE	4 		/* base time between retries */
#define MAXRETRY	3		/* max number of retries per addr */
#define MAXCNAMES	8		/* max # of CNAMES tried per addr */
#define MAXQUERIES	20		/* max # of queries to be made */
					/* (prevent "recursive" loops) */
#define	INIT_REFRESH	600		/* retry time for initial secondary */
					/* contact (10 minutes) */

#define XFER_TIMER            120	   /* named-xfer's connect timeout */
#define MAX_XFER_TIME         60 * 60 * 2  /* max seconds for an xfer */
#define XFER_TIME_FUDGE	      10           /* MAX_XFER_TIME fudge */
#define MAX_XFERS_RUNNING     4            /* max value of xfer_running_cnt */

#define ALPHA    0.7	/* How much to preserve of old response time */
#define	BETA	 1.2	/* How much to penalize response time on failure */
#define	GAMMA	 0.98	/* How much to decay unused response times */

/* these fields are ordered to maintain word-alignment;
 * be careful about changing them.
 */
struct zoneinfo {
	char	*z_origin;		/* root domain name of zone */
	time_t	z_time;			/* time for next refresh */
	time_t	z_lastupdate;		/* time of last refresh */
	u_int32_t	z_refresh;		/* refresh interval */
	u_int32_t	z_retry;		/* refresh retry interval */
	u_int32_t	z_expire;		/* expiration time for cached info */
	u_int32_t	z_minimum;		/* minimum TTL value */
	u_int32_t	z_serial;		/* changes if zone modified */
	char	*z_source;		/* source location of data */
	time_t	z_ftime;		/* modification time of source file */
	struct	in_addr z_addr[NSMAX];	/* list of master servers for zone */
	u_char	z_addrcnt;		/* number of entries in z_addr[] */
	u_char	z_type;			/* type of zone; see below */
	u_short	z_state;		/* state bits; see below */
	u_int	z_xferpid;		/* xfer child pid */
};

	/* zone types (z_type) */
#define	Z_NIL		0		/* zone slot not in use */
#define Z_PRIMARY	1
#define Z_SECONDARY	2
#define Z_CACHE		3

	/* zone state bits */
#define	Z_AUTH		0x01		/* zone is authoritative */
#define	Z_NEED_XFER	0x02		/* waiting to do xfer */
#define	Z_XFER_RUNNING	0x04		/* asynch. xfer is running */
#define	Z_NEED_RELOAD	0x08		/* waiting to do reload */
#define	Z_SYSLOGGED	0x10		/* have logged timeout */
#define	Z_UNUSED	0x20		/*  */
#define	Z_FOUND		0x40		/* found in boot file when reloading */
#define	Z_INCLUDE	0x80		/* set if include used in file */
#define	Z_DB_BAD	0x100		/* errors when loading file */
#define	Z_TMP_FILE	0x200		/* backup file for xfer is temporary */
#ifdef ALLOW_UPDATES
#define	Z_DYNAMIC	0x400		/* allow dynamic updates */
#define	Z_DYNADDONLY	0x800		/* dynamic mode: add new data only */
#define	Z_CHANGED	0x1000		/* should replace hasChanged */
#endif /* ALLOW_UPDATES */

	/* xfer exit codes */
#define	XFER_UPTODATE	0		/* zone is up-to-date */
#define	XFER_SUCCESS	1		/* performed transfer successfully */
#define	XFER_TIMEOUT	2		/* no server reachable/xfer timeout */
#define	XFER_FAIL	3		/* other failure, has been logged */

/*
 * Structure for recording info on forwarded queries.
 */
struct qinfo {
	u_short	q_id;			/* id of query */
	u_short	q_nsid;			/* id of forwarded query */
	struct	sockaddr_in q_from;	/* requestor's address */
	char	*q_msg;			/* the message */
	short	q_msglen;		/* len of message */
	short	q_dfd;			/* UDP file descriptor */
	short	q_naddr;		/* number of addr's in q_addr */
	short	q_curaddr;		/* last addr sent to */
	struct	fwdinfo	*q_fwd;		/* last	forwarder used */
	time_t	q_time;			/* time to retry */
	time_t	q_expire;		/* time to expire */
	struct	qinfo *q_next;		/* rexmit list (sorted by time) */
	struct	qinfo *q_link;		/* storage list (random order) */
	struct  qserv {
		struct	sockaddr_in ns_addr;	/* addresses of NS's */
		struct  databuf *ns;	/* databuf for NS record */
		struct  databuf *nsdata; /* databuf for server address */
		struct  timeval stime;	/* time first query started */
		int	nretry;		/* # of times addr retried */
	} q_addr[NSMAX];		/* addresses of NS's */
	struct	databuf *q_usedns[NSMAX]; /* databuf for NS that we've tried */
	short	q_nusedns;
	short	q_cname;		/* # of cnames found */
	short	q_nqueries;		/* # of queries required */
	short	q_cmsglen;		/* len of cname message */
	char	*q_cmsg;		/* the cname message */
	struct	qstream *q_stream;	/* TCP stream, null if UDP */
	int	q_system;		/* boolean, system query */
};

#define	Q_NEXTADDR(qp,n)	\
	(((qp)->q_fwd == (struct fwdinfo *)0) ? \
	 &(qp)->q_addr[n].ns_addr : &(qp)->q_fwd->fwdaddr)

#define	RETRY_TIMEOUT	45
#define PRIMING_CACHE	42
#define QINFO_NULL	((struct qinfo *)0)

#ifndef XFER
extern struct qinfo *qfindid();
#ifdef DMALLOC
extern struct qinfo *qnew_tagged();
#define qnew() qnew_tagged(__FILE__, __LINE__)
#else
extern struct qinfo *qnew();
#endif
extern struct qinfo *retryqp;		/* next query to retry */
#endif /* XFER */

/*
 * Return codes from ns_forw:
 */
#define	FW_OK		0
#define	FW_DUP		1
#define	FW_NOSERVER	2
#define	FW_SERVFAIL	3

struct qstream {
	int 	s_rfd;			/* stream file descriptor */
	int 	s_size;			/* expected amount of data to recive */
	int 	s_bufsize;		/* amount of data recived in s_buf */
	u_char  *s_buf;			/* buffer of received data */
	u_char  *s_bufp;		/* pointer into s_buf of recived data*/
	struct	qstream *s_next;	/* next stream */
	struct	sockaddr_in s_from;	/* address query came from */
	u_int32_t	s_time;			/* time stamp of last transaction */
	int	s_refcnt;		/* number of outstanding queries */
	u_short	s_tempsize;		/* temporary for size from net */
};

#define QSTREAM_NULL	((struct qstream *)0)
extern struct qstream *streamq;		/* stream queue */

struct qdatagram {
	int 	dq_dfd;			/* datagram file descriptor */
	struct	qdatagram *dq_next;	/* next datagram */
	struct	in_addr  dq_addr;	/* address of interface */
};

#define QDATAGRAM_NULL	((struct qdatagram *)0)
extern struct qdatagram *datagramq;	/* datagram queue */

struct netinfo {
	struct netinfo *next;
	u_int32_t net;
	u_int32_t mask;
	struct in_addr my_addr;
};
extern struct netinfo *net_on_netlist();
#define ALLOW_NETS	0x0001
#define	ALLOW_HOSTS	0x0002
#define	ALLOW_ALL	(ALLOW_NETS | ALLOW_HOSTS)

struct fwdinfo {
	struct fwdinfo *next;
	struct sockaddr_in fwdaddr;
};

/*
 *  Statistics Defines
 */
struct stats {
	u_int32_t	cnt;
	char	*description;
};

/* gross count of UDP packets in and out */
#define	S_INPKTS	0
#define	S_OUTPKTS	1
/* gross count of queries and inverse queries received */
#define	S_QUERIES	2
#define	S_IQUERIES	3
#define S_DUPQUERIES	4
#define	S_RESPONSES	5
#define	S_DUPRESP	6
#define	S_RESPOK	7
#define	S_RESPFAIL	8
#define	S_RESPFORMERR	9
#define	S_SYSQUERIES	10
#define	S_PRIMECACHE	11
#define	S_CHECKNS	12
#define	S_BADRESPONSES	13
#define	S_MARTIANS	14
#define S_NSTATS	15	/* Careful! */
#ifdef STATS
extern struct stats stats[S_NSTATS];
extern unsigned long typestats[T_ANY+1];
#endif

#ifdef DEBUG
extern int debug;			/* debug flag */
extern FILE *ddt;			/* debug file discriptor */
#endif
#ifdef QRYLOG
extern int qrylog;			/* query log flag */
#endif /*QRYLOG*/

#ifndef XFER
extern int ds;				/* datagram socket */
extern struct timeval tt;		/* place to store time */

extern struct zoneinfo *zones;		/* zone information */
extern int nzones;			/* number of zones in use */

extern int forward_only;		/* true on slave server */

extern u_int32_t net_mask();
extern struct qinfo *sysquery();
#endif /* XFER */

extern int my_close(), my_fclose();	/* db_glue.c */
