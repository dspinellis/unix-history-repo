/* $Header: /f/osi/others/ntp/RCS/ntp.h,v 7.2 91/02/22 09:33:43 mrose Interim $ */

/*
 *  $Log:	ntp.h,v $
 * Revision 7.2  91/02/22  09:33:43  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/12/10  23:15:43  mrose
 * isode/
 * 
 * Revision 7.0  90/12/10  17:21:24  mrose
 * *** empty log message ***
 * 
 * Revision 1.4  90/08/14  10:13:51  jpo
 * new protocol version
 * 
 * Revision 1.3  90/02/13  14:23:52  jpo
 * First beta released version
 * 
 * Revision 1.2  89/12/19  08:32:34  jpo
 * Updated for ISODE 6.0ish
 * 
 * Revision 1.1  89/06/15  20:36:54  jpo
 * Initial revision
 * 
 * 
 */
#include "ntp-config.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/file.h>
#ifdef NOSWAP
#include <sys/lock.h>
#endif

#include <net/if.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/udp.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <strings.h>
#include <errno.h>
#include <nlist.h>

#include "rosy.h"
#include "tsap.h"
#include "logger.h"

#ifndef FD_SET
#define	NFDBITS		32
#define	FD_SETSIZE	32
#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))
#endif

#ifndef	NBBY
#define	NBBY	8	/* number of bits per byte */
#endif

#ifdef DEBUG
#define TRACE(level, args)     if (debug >= level) \
	SLOG (pgm_log, LLOG_DEBUG, NULLCP, args); \
	else
#else
#define TRACE(level, args)
#endif

struct Naddr {
	int type;
	union {
		struct sockaddr_in un_inet_ad;
		struct PSAPaddr un_psap_ad;
	} un;
#define inet_ad un.un_inet_ad
#define psap_ad un.un_psap_ad
};

typedef struct Refid {
	char	rid_type;
#define RID_STRING 1
#define RID_INET 2
#define RID_PSAP 3
	union {
		char un_rid_string[5];
		u_long un_rid_inet;
		struct PSAPaddr un_rid_psap;
	} un;
#define rid_string un.un_rid_string
#define rid_inet un.un_rid_inet
#define rid_psap un.un_rid_psap
} Refid;

#define	MAXNETIF	10

struct intf {
	int fd;
	char *name;
	struct Naddr addr;
	struct sockaddr_in bcast;
	struct sockaddr_in mask;
	int uses;
	int flags;
#define INTF_VALID	01
#define INTF_STATIC	02
#define INTF_PENDING	04
#define INTF_ACCEPTING	010
#define INTF_SELECT	(INTF_ACCEPTING|INTF_PENDING|INTF_VALID)
	int if_flags;
	char	*vec[4];
	int	vecp;
	int	inum;
};
extern struct intf *addrs;
extern int nintf;
extern struct intf *getintf ();

#define	ACT_ERROR	1
#define	ACT_RECV	2
#define	ACT_XMIT	3
#define	ACT_PKT		4
extern char actions[5][5];

/*
 *  Definitions for the masses
 */
#define	JAN_1970	2208988800	/* 1970 - 1900 in seconds */

/*
 *  Daemon specific (ntpd.c)
 */

#define	SHIFT_MASK	0xff	/* number of intervals to wait */

struct list {
	struct ntp_peer *head;
	struct ntp_peer *tail;
	int members;
};

#define	STRMCMP(a, cond, b) \
	(((a) == UNSPECIFIED ? NTP_INFIN+1 : a) cond \
	 ((b) == UNSPECIFIED ? NTP_INFIN+1 : (b)))


/*
 *  Definitions outlined in the NTP spec
 */
#define	NTP_VERSION	1
#define	NTP_PORT	123	/* for ref only (see /etc/services) */
#define	NTP_INFIN	15
#define	NTP_MAXAGE	86400
#define	NTP_MAXSKW	0.01	/* seconds */
#define	NTP_MINDIST	0.02	/* seconds */
#ifdef	REFCLOCK
#define	NTP_REFMAXSKW	0.001	/* seconds (for REFCLOCKs) */
#define	NTP_REFMINDIST	0.001	/* seconds (for REFCLOCKs) */
#endif
#define	NTP_MINPOLL	6	/* (64) seconds between messages */
#define	NTP_MAXPOLL	10	/* (1024) secs to poll */
#define	NTP_WINDOW	8	/* size of shift register */
#define	NTP_MAXWGT	8	/* maximum allowable dispersion */
#define	NTP_MAXLIST	5	/* max size of selection list */
#define	NTP_MAXSTRA	2	/* max number of strata in selection list */
#define	X_NTP_CANDIDATES 64	/* number of peers to consider when doing
				   clock selection */
#define NTP_SELECT	0.75	/* weight used to compute dispersion */

#define	PEER_MAXDISP	64.0	/* Maximum dispersion  */
#define	PEER_THRESHOLD	0.5	/* dispersion threshold */
#define	PEER_FILTER	0.5	/* filter weight */

#define BACKOFF_COUNT	3	/* backoff for lost peers */
#if	XTAL == 0
#define	PEER_SHIFT	4
#define	NTP_WINDOW_SHIFT_MASK 0x0f
#else
#define	PEER_SHIFT	8
#define	NTP_WINDOW_SHIFT_MASK 0xff
#endif


/*
 *  5.1 Uniform Phase Adjustments
 *  Clock parameters
 */
#define	CLOCK_UPDATE	8	/* update interval (1<<CLOCK_UPDATE secs) */
#if	XTAL
#define	CLOCK_ADJ	2	/* adjustment interval (1<<CLOCK_ADJ secs) */
#define	CLOCK_PHASE	8	/* phase shift */
#define	CLOCK_MAX	0.128	/* maximum aperture (milliseconds) */
#else
#define	CLOCK_ADJ	0
#define	CLOCK_PHASE	6	/* phase shift */
#define	CLOCK_MAX	0.512	/* maximum aperture (milliseconds) */
#endif
#define	CLOCK_FREQ	10	/* frequency shift */
#define	CLOCK_TRACK	8
#define	CLOCK_COMP	4
#define	CLOCK_FACTOR	18

/*
 * Structure definitions for NTP fixed point values
 *
 *    0			  1		      2			  3
 *    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |			       Integer Part			     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |			       Fraction Part			     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *    0			  1		      2			  3
 *    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |		  Integer Part	     |	   Fraction Part	     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*/
struct l_fixedpt {
	u_long int_part;
	u_long fraction;
};

struct s_fixedpt {
	u_short int_part;
	u_short fraction;
};

/*  =================  Table 3.3. Packet Variables   ================= */
/*
 *    0			  1		      2			  3
 *    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |LI | VN  | Mode|	  Stratum    |	    Poll     |	 Precision   |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |			   Synchronizing Distance		     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |			  Synchronizing Dispersion		     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |			Reference Clock Identifier		     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |								     |
 *   |		       Reference Timestamp (64 bits)		     |
 *   |								     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |								     |
 *   |		       Originate Timestamp (64 bits)		     |
 *   |								     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |								     |
 *   |			Receive Timestamp (64 bits)		     |
 *   |								     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |								     |
 *   |			Transmit Timestamp (64 bits)		     |
 *   |								     |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*/
struct ntpdata {
	u_char status;		/* status of local clock and leap info */
	u_char stratum;		/* Stratum level */
	u_char ppoll;		/* poll value */
	int precision:8;
	struct s_fixedpt distance;
	struct s_fixedpt dispersion;
	u_long refid;
	struct l_fixedpt reftime;
	struct l_fixedpt org;
	struct l_fixedpt rec;
	struct l_fixedpt xmt;
#ifdef notdef
/* Not Yet in this version... */
	u_long keyid;
	struct l_fixedpt mac;
#endif
};
#define MAC_LEN         (sizeof(l_fp) + sizeof(u_long))
#define LEN_PKT_MAC     (sizeof(struct pkt))
#define LEN_PKT_NOMAC   (sizeof(struct pkt) - MAC_LEN)

/*
 *	Leap Second Codes (high order two bits)
 */
#define	NO_WARNING	0x00	/* no warning */
#define	PLUS_SEC	0x40	/* add a second (61 seconds) */
#define	MINUS_SEC	0x80	/* minus a second (59 seconds) */
#define	ALARM		0xc0	/* alarm condition (clock unsynchronized) */

/*
 *	Clock Status Bits that Encode Version
 */
#define VERS2PKT(status,vers)	((status) |= ((vers) << 3))
#define PKT2VERS(status)	((status >> 3)& 07)
#define	NTPVERSION_1	0x08
#define	VERSIONMASK	0x38
#define LEAPMASK	0xc0
#define	MODEMASK	0x07

/*
 *	Code values
 */
#define	MODE_UNSPEC	0	/* unspecified */
#define	MODE_SYM_ACT	1	/* symmetric active */
#define	MODE_SYM_PAS	2	/* symmetric passive */
#define	MODE_CLIENT	3	/* client */
#define	MODE_SERVER	4	/* server */
#define	MODE_BROADCAST	5	/* broadcast */
#define	MODE_RES1	6	/* reserved */
#define	MODE_RES2	7	/* reserved */

/*
 *	Stratum Definitions
 */
#define	UNSPECIFIED	0
#define	PRIM_REF	1	/* radio clock */
#define	INFO_QUERY	62	/* **** THIS implementation dependent **** */
#define	INFO_REPLY	63	/* **** THIS implementation dependent **** */


/* =================  table 3.2 Peer Variables	================= */
struct ntp_peer {
	struct ntp_peer *next, *prev;
	struct Naddr src;		/* both peer.srcadr and 
					   peer.srcport */
	int	flags;			/* local flags */
#define	PEER_FL_CONFIG		0x0001
#define	PEER_FL_AUTHENABLE	0x0002
#define PEER_FL_SNOOZE		0x0004
#define	PEER_FL_SANE		0x0100	/* sane peer */
#define	PEER_FL_CANDIDATE	0x0200	/* candidate peer */
#define	PEER_FL_SYNC		0x1000	/* peer can bet sync'd to */
#define	PEER_FL_BCAST		0x2000	/* broadcast peer */
#define	PEER_FL_REFCLOCK	0x4000	/* peer is a local reference clock */
#define	PEER_FL_SELECTED	0x8000	/* actually used by query routine */
#define PEER_FL_CONREQ		0x10000
#define PEER_FL_CONNECTED	0x20000	/* connected */
#define PEER_FL_CONINP1		0x40000 /* connection in progress - stage1 */
#define PEER_FL_CONINP2		0x80000 /* connection in progress - stage2 */
#define PEER_FL_CONNSTATE	(PEER_FL_CONINP2|PEER_FL_CONINP1|PEER_FL_CONNECTED)
	int	sock;			/* index into sockets to derive
					   peer.dstadr and peer.dstport */
	u_char	leap;			/* receive */
	u_char	hmode;			/* receive */
	u_char	stratum;		/* receive */
	u_char	ppoll;			/* receive */
	u_char	hpoll;			/* poll update */
	u_char	vers;			/* version */
#define PEERMODE_NORMAL	1
#define PEERMODE_QUERY  2
	u_char  mode;			/* mode */
	short	precision;		/* receive */
	struct	s_fixedpt distance;	/* receive */
	struct	s_fixedpt dispersion;	/* receive */
	Refid	refid;			/* receive */
	struct	l_fixedpt reftime;	/* receive */
	struct	l_fixedpt org;		/* receive, clear */
	struct	l_fixedpt rec;		/* receive, clear */
	struct	l_fixedpt xmt;		/* transmit, clear */
	u_long	reach;			/* receive, transmit, clear */
	u_long	valid;			/* packet, transmit, clear */
	u_long	timer;			/* receive, transmit, poll update */
	long	stopwatch;		/* <<local>> for timing */
	/*
	 * first order offsets
	 */
	struct	filter {
		short samples;		/* <<local>> */
		double offset[PEER_SHIFT];
		double delay[PEER_SHIFT];
	} filter;			/* filter, clear */

	double	estdelay;		/* filter */
	double	estoffset;		/* filter */
	double	estdisp;		/* filter */

	u_long	pkt_sent;		/* <<local>> */
	u_long 	pkt_rcvd;		/* <<local>> */
	u_long	pkt_dropped;		/* <<local>> */
	int	backoff;		/* <<local>> */
};

/* ================= table 3.1:  System Variables ================= */

struct sysdata {			/* procedure */
	u_char leap;			/* clock update */
	u_char stratum;			/* clock update */
	short precision;		/* system */
	struct s_fixedpt distance;	/* clock update */
	struct s_fixedpt dispersion;	/* clock update */
	Refid refid;			/* clock update */
	struct l_fixedpt reftime;	/* clock update */
	int hold;			/* clock update */
	struct ntp_peer *peer;		/* selection */
	int maxpeers;			/* <<local>> */
	u_char filler;		/* put here for %&*%$$ SUNs */
};

#define	NTPDC_VERSION	2

/*
 *  These structures are used to pass information to the ntpdc (control)
 *  program.  They are unique to this implementation and not part of the
 *  NTP specification.
 */
struct clockinfo {
	u_long net_address;
	u_long my_address;
	u_short port;
	u_short flags;
	u_long pkt_sent;
	u_long pkt_rcvd;
	u_long pkt_dropped;
	u_long timer;
	u_char leap;
	u_char stratum;
	u_char ppoll;
	int precision:8;

	u_char hpoll;
	u_char filler1;
	u_short reach;

	long	estdisp;			/* scaled by 1000 */
	long	estdelay;			/* in milliseconds */
	long	estoffset;			/* in milliseconds */
	u_long refid;
	struct l_fixedpt reftime;
	struct info_filter {
		short index;
		short filler;
		long offset[PEER_SHIFT];	/* in milliseconds */
		long delay[PEER_SHIFT];		/* in milliseconds */
	} info_filter;
};

struct ntpinfo {
	u_char version;
	u_char type;		/* request type (stratum in ntp packets) */
	u_char count;		/* number of entries in this packet */
	u_char seq;		/* sequence number of this packet */

	u_char npkts;		/* total number of packets */
	u_char peers;
	u_char fill3;
	u_char fill4;
};

extern int selfds;

extern fd_set globmask, globwmask;

extern char *paddr (), *ntoa ();
extern char *strcpy (), *strcat ();
extern char *sprintf ();
extern char *malloc ();

extern long lseek ();
extern long random ();

extern void advise (), avoid ();

extern double s_fixed_to_double(), ul_fixed_to_double();
extern double	atof();

#ifdef DEBUG
extern void	dump_pkt ();
extern int debug;
#endif
