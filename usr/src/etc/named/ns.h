/*	ns.h	4.3	86/06/04	*/

/*
 * Copyright (c) 1985 Regents of the University of California
 *	All Rights Reserved
 */

/*
 * Global definitions and variables for the name server.
 */

#include <strings.h>
#include <arpa/inet.h>

/* MAXRETRY * RETRYTIME * #ns should be less than 1 minute or so */
/* 3 * 8 * 3 = 72 seconds for 3 root domain servers */
#define MAXNS		10		/* max number of NS's to try */
#define RETRYTIME	8 		/* time between retries */
#define MAXRETRY	3		/* max number of retries per addr */
#define MAXCNAMES	8		/* max # of CNAMES tried per addr */

#define MAXZONES 32

struct zoneinfo {
	int	z_type;			/* type of zone */
	char	*z_origin;		/* root domain name of zone */
	time_t	z_time;			/* time left before refresh */
	u_long	z_refresh;		/* refresh interval */
	u_long	z_retry;		/* refresh retry interval */
	u_long	z_expire;		/* expiriation time for cached info */
	u_long	z_minimum;		/* minimum TTL value */
	u_long	z_serial;		/* changes if zone modified */
	char	*z_source;		/* source location of data */
	int	z_addrcnt;		/* address count */
	struct	in_addr z_addr[MAXNS];	/* list of master servers for zone */
};

	/* zone types (z_type) */
#define Z_PRIMARY	1
#define Z_SECONDARY	2
#define Z_CACHE		3
#define Z_DOMAIN	4

	/* Flags to ns_req() */
#define	ISTCP		01
#define ISLOCAL		02

/*
 * Structure for recording info on forwarded queries.
 */
struct qinfo {
	u_short	q_id;			/* id of query */
	u_short	q_nsid;			/* id of forwarded query */
	struct	sockaddr_in q_from;	/* requestor's address */
	char	*q_msg;			/* the message */
	int	q_msglen;		/* len of message */
	int	q_naddr;		/* number of addr's in q_addr */
	int	q_curaddr;		/* last addr sent to */
	time_t	q_time;			/* time to retry */
	struct	qinfo *q_next;		/* rexmit list (sorted by time) */
	struct	qinfo *q_link;		/* storage list (random order) */
	struct	sockaddr_in q_addr[MAXNS];	/* addresses of NS's */
	int	q_nretry[MAXNS];	/* # of times addr retried */
	int	q_cname;		/* # of cnames found */
	char	*q_cmsg;		/* the cname message */
	int	q_cmsglen;		/* len of cname message */
	struct	qstream *q_stream;	/* TCP stream, null if UDP */
};

#define QINFO_NULL	((struct qinfo *)0)
extern struct qinfo *qfindid();
extern struct qinfo *qnew();
extern struct qinfo *retryqp;		/* next query to retry */

struct qstream {
	int 	s_rfd;			/* stream file descriptor */
	int 	s_size;			/* expected amount of data to recive */
	int 	s_bufsize;		/* amount of data recived in s_buf */
	char    *s_buf;			/* buffer of recived data */
	char    *s_bufp;		/* pointer into s_buf of recived data */
	struct	qstream *s_next;	/* next stream */
	struct	sockaddr_in s_from;	/* address query came from */
	u_long	s_time;			/* time stamp of last transaction */
	int	s_refcnt;		/* number of outstanding queries */
	u_short	s_tempsize;		/* temporary for size from net */
};

#define QSTREAM_NULL	((struct qstream *)0)
extern struct qstream *streamq;		/* stream queue */

#ifdef DEBUG
extern int debug;			/* debug flag */
extern FILE *ddt;			/* debug file discriptor */
#endif
extern int ds;				/* datagram socket */
extern struct timeval tt;		/* place to store time */

extern struct itimerval ival;		/* maintenance interval */
extern struct zoneinfo zones[MAXZONES];	/* zone information */
extern int nzones;			/* number of zones in use */

#ifdef vax
extern u_short htons(), ntohs();
extern u_long htonl(), ntohl();
#endif
