/* tpkt.h - include file for transport providers (TS-PROVIDER) */

/* 
 * $Header: /f/osi/h/RCS/tpkt.h,v 7.4 91/02/22 09:25:14 mrose Interim $
 *
 *
 * $Log:	tpkt.h,v $
 * Revision 7.4  91/02/22  09:25:14  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/07/27  08:44:50  mrose
 * update
 * 
 * Revision 7.2  90/03/23  17:30:54  mrose
 * 8
 * 
 * Revision 7.1  89/12/07  01:07:57  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  21:56:07  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "tsap.h"		/* definitions for TS-USERs */

/*  */


#define	tsapPsig(tb, sd) \
{ \
    if ((tb = findtblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return tsaplose (td, DR_PARAMETER, NULLCP, \
			    "invalid transport descriptor"); \
    } \
    if (!(tb -> tb_flags & TB_CONN)) { \
	(void) sigiomask (smask); \
	return tsaplose (td, DR_OPERATION, NULLCP, \
			    "transport descriptor not connected"); \
    } \
}

#define	missingP(p) \
{ \
    if (p == NULL) \
	return tsaplose (td, DR_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

#define	toomuchP(b,n,m,p) \
{ \
    if (b == NULL) \
	n = 0; \
    else \
	if (n > m) \
	    return tsaplose (td, DR_PARAMETER, NULLCP, \
			    "too much %s user data, %d octets", p, n); \
}

#define	copyTPKTdata(t,base,len) \
{ \
    register struct udvec *uv = t -> t_udvec; \
    if (len > 0) \
	uv -> uv_base = base, uv -> uv_len = len, uv++; \
    uv -> uv_base = NULL; \
}

#ifndef	lint
#ifndef	__STDC__
#define	copyTSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}
#else
#define	copyTSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}
#endif
#else
#define	copyTSAPdata(base,len,d)	bcopy (base, (char *) d, len)
#endif


int	tpktlose (), tsaplose ();

/*  */

struct tsapADDR {
    struct NSAPaddr ta_addr;
    int	    ta_present;

    int	    ta_selectlen;
    
    union {
	char	ta_un_selector[TSSIZE];

	u_short	ta_un_port;
    }		un_ta;
};

				/* network type codes:
					must be outside [0-9A-Fa-f] */
#define	NT_TCP	'T'		/* TCP */
#define	NT_X25	'X'		/* X.25 */
#define	NT_BRG	'G'		/* Bridge */
#define	NT_BSD	'Z'		/* 4.4BSD */
#define	NT_SUN	'S'		/* SunLink OSI */


struct tsapblk {
    struct tsapblk *tb_forw;	/* doubly-linked list */
    struct tsapblk *tb_back;	/*   .. */

    int     tb_fd;		/* file descriptor */

    char    tb_flags;		/* our state */
#define	TB_NULL		0x00
#define	TB_CONN		0x01	/* connected */
#define	TB_ASYN		0x02	/* asynchronous */
#define	TB_EXPD		0x04	/* expedited transfer selected */
#define	TB_TCP		0x08	/* underlying service is TCP */
#define	TB_X25		0x10	/*   ..		      is X.25 */
#define TB_BRG		0x20	/*   ..		      is Bridge */
#define	TB_TP0		(TB_TCP | TB_X25 | TB_BRG)
#define	TB_TP4		0x40	/*   ..		      is TP4 */
				/* all TP4's use this value as it make
				   sense to have only one TP4 service
				   compiled in... */
#define	TB_QWRITES	0x80	/* queued writes OK */

    char   *tb_magic;		/* generic pointer */

				/* saved retry variables */
    char  *tb_data;		/* saved user data */
    int    tb_cc;		/* saved user data count */
    char   tb_expedited;	/* saved expedited */
    struct TSAPaddr *tb_called;	/* saved addresses */
    struct TSAPaddr *tb_calling;/*   .. */

    struct tsapkt *tb_retry;	/* initial tpkt */

    u_short tb_srcref;		/* source reference */
    u_short tb_dstref;		/* destination reference */

    int	    tb_tsdusize;	/* maximum TSDU size */
    int	    tb_tpduslop;	/*   .. */
    int	    tb_tpdusize;	/* for tp0ts */

    int	    tb_sent;		/* TPDU bytes sent */
    int	    tb_recv;		/* TPDU bytes recv */

    struct QOStype tb_qos;	/* quality of service */

    struct qbuf tb_qbuf;	/* for segmented TSDUs */
    int	    tb_len;		/*   .. */
    
    struct tsapADDR tb_initiating;/* initiator */
    struct tsapADDR tb_responding;/* responder */

    IFP	    tb_retryfnx;	/* resume async connection */

    IFP	    tb_connPfnx;	/* TP connect */
    IFP	    tb_retryPfnx;	/* TP retry connect */
    IFP	    tb_startPfnx;	/* TP start accept */
    IFP	    tb_acceptPfnx;	/* TP accept */
    IFP	    tb_writePfnx;	/* TP write data */
    IFP	    tb_readPfnx;	/* TP read data */
    IFP	    tb_discPfnx;	/* TP disconnect */
    IFP	    tb_losePfnx;	/* TP loses */

    IFP	    tb_drainPfnx;	/* TP drain queued writes */
    IFP	    tb_queuePfnx;	/* TP note queued writes */
    struct qbuf tb_qwrites;	/* queue of writes to retry */

    IFP	    tb_initfnx;		/* init for read from network */
    IFP	    tb_readfnx;		/* read from network */
    IFP	    tb_writefnx;	/* write to network */
    IFP	    tb_closefnx;	/* close network */
    IFP	    tb_selectfnx;	/* select network */
    IFP	    tb_checkfnx;	/* check network prior to select */
    IFP	    tb_nreadfnx;	/* estimate of octets waiting to be read */

    IFP	    tb_DataIndication;	/* INDICATION handlers */
    IFP     tb_DiscIndication;	/*   .. */

#ifdef  MGMT
    IFP     tb_manfnx;          /* for management reports */
    int     tb_pdus;            /* PDUs sent */
    int     tb_pdur;            /* PDUs recv */
    int     tb_bytes;           /* bytes sent since last report */
    int     tb_byter;           /* bytes recv        ..         */
#endif
};
#define	NULLBP		((struct tsapblk *) 0)


int	freetblk ();
struct tsapblk *newtblk (), *findtblk ();

/*    TPKT datastructure */

struct tsapkt {
    int		t_errno;

    struct {
	u_char    pk_vrsn;
#define	TPKT_VRSN	3

	u_char    pk_rsrvd;

	u_short   pk_length;
#define	TPKT_MAXLEN	0xffff
    }       t_pkthdr;
#define	t_vrsn		t_pkthdr.pk_vrsn
#define	t_rsrvd		t_pkthdr.pk_rsrvd
#define	t_length	t_pkthdr.pk_length

#define TPKT_HDRLEN(t) (sizeof ((t) -> t_pkthdr) + sizeof ((t) -> t_li) \
			    + sizeof ((t) -> t_code))

    struct {
	u_char    tp_li;
#ifndef	lint
#ifndef	__STDC__
#define	TPDU_MINLEN(t,c)	(c/* */_SIZE(t) + sizeof ((t) -> t_code))
#else
#define	TPDU_MINLEN(t,c)	(c##_SIZE(t) + sizeof ((t) -> t_code))
#endif
#else
#define	TPDU_MINLEN(t,c)	(sizeof ((t) -> t_code))
#endif
#define	TPDU_MAXLEN(t)	\
	(min (0xfe, (t) -> t_length - sizeof ((t) -> t_pkthdr) \
				    - sizeof ((t) -> t_li)))
#define	TPDU_USRLEN(t)	\
	((t) -> t_length - sizeof ((t) -> t_pkthdr) \
			 - sizeof ((t) -> t_li) - (t) -> t_li)

	u_char    tp_code;
#define	TPDU_CODE(t)	((t) -> t_code & 0xf0)
#define	TPDU_CR		0xe0	/* CONNECTION REQUEST */
#define	TPDU_CC		0xd0	/* CONNECTION CONFIRMATION */
#define	TPDU_DR		0x80	/* DISCONNECT REQUEST */
#define	TPDU_DC		0xc0	/* DISCONNECT CONFIRMATION */
#define	TPDU_DT		0xf0	/* DATA */
#define	TPDU_ED		0x10	/* EXPEDITED DATA */
#define	TPDU_AK		0x60	/* ACKNOWLEDGE */
#define	TPDU_EA		0x20	/* EXPEDITED ACKNOWLEDGE */
#define	TPDU_RJ		0x50	/* REJECT */
#define	TPDU_ER		0x70	/* ERROR */

	union {
	    struct {
				/* FIXED part */
		u_short   un_cr_dstref;
		u_short   un_cr_srcref;

		u_char    un_cr_class;
#define	CR_CLASS(t)	((t) -> t_cr.cr_class & 0xf0)
#define	CR_CLASS_TP0	0x00	/* class 0 */
#define	CR_CLASS_TP1	0x10	/*   ..  1 */
#define	CR_CLASS_TP2	0x20	/*   ..  2 */
#define	CR_CLASS_TP3	0x30	/*   ..  3 */
#define	CR_CLASS_TP4	0x40	/*   ..  4 */
#define	CR_OPT_EXTD	0x02	/* extended formats in classes 2-4 */
#define	CR_OPT_EXPL	0x01	/* explicit flow control in class 2 */

				/* VARIABLE part */
		char	  un_cr_called[TSSIZE];
		int	  un_cr_calledlen;
		
		char	  un_cr_calling[TSSIZE];
		int	  un_cr_callinglen;

		u_char    un_cr_tpdusize;

		u_short	  un_cr_options;

		u_char	  un_cr_alternate;
	    }       un_cr;
#define	cr_dstref	un_cr_dstref
#define	cr_srcref	un_cr_srcref
#define	cr_class	un_cr_class
#define	cr_tpdusize	un_cr_tpdusize
#define	cr_options	un_cr_options
#define	cr_alternate	un_cr_alternate
#define	CR_SIZE(t)	5

#define	un_cc		un_cr
#define	cc_dstref	un_cr_dstref
#define	cc_srcref	un_cr_srcref
#define	cc_class	un_cr_class
#define	cc_tpdusize	un_cr_tpdusize
#define	cc_options	un_cr_options
#define	CC_SIZE(t)	5

	    struct {
				/* FIXED part */
		u_short   un_dr_dstref;
		u_short   un_dr_srcref;
		u_char    un_dr_reason;
	    }       un_dr;
#define	dr_dstref	un_dr_dstref
#define	dr_srcref	un_dr_srcref
#define	dr_reason	un_dr_reason
#define	DR_SIZE(t)	5

	    struct {
				/* FIXED part */
		u_char   un_dt_nr;
#define	DT_EOT		0x80
	    }       un_dt;
#define	dt_nr		un_dt_nr
#define	DT_SIZE(t)	1
#define	DT_MAGIC	(2 + 1)

/* Expedited service is not allowed in TP0, but for testing purposes,
   we permit it when the underlying service is TCP.  Note we use a
   non-standard packet format (identical to the DT format).
 */
#define	un_ed		un_dt
#define	ed_nr		un_dt_nr
#define	ED_SIZE(t)	1

	    struct {
				/* FIXED part */
		u_short	   un_er_dstref;
		u_char	   un_er_reject;
#define	ER_REJ_NOTSPECIFIED	0x00	/* Reason not specified */
#define	ER_REJ_CODE		0x01	/* Invalid parameter code */
#define	ER_REJ_TPDU		0x02	/* Invalid TPDU type */
#define	ER_REJ_VALUE		0x03	/* Invalid parameter value */
	    }	    un_er;
#define	er_dstref	un_er_dstref
#define	er_reject	un_er_reject
#define	ER_SIZE(t)	3
	}       tp_un;
#define	tp_cr		tp_un.un_cr
#define	tp_cc		tp_un.un_cc
#define	tp_dr		tp_un.un_dr
#define	tp_dt		tp_un.un_dt
#define	tp_ed		tp_un.un_ed
#define	tp_er		tp_un.un_er

	int		tp_vlen;
	char	       *tp_vbase;

	struct qbuf    *tp_qbuf;	/* fd2tpkt ONLY */

#define	NTPUV		12		/* really should be
						MSG_MAXIOVLEN - 4 */
	struct udvec tp_udvec[NTPUV];	/* tpkt2fd ONLY */
    }               t_tpdu;
#define	t_li		t_tpdu.tp_li
#define	t_code		t_tpdu.tp_code
#define	t_cr		t_tpdu.tp_un.un_cr
#define	t_called	t_tpdu.tp_un.un_cr.un_cr_called
#define	t_calledlen	t_tpdu.tp_un.un_cr.un_cr_calledlen
#define	t_calling	t_tpdu.tp_un.un_cr.un_cr_calling
#define	t_callinglen	t_tpdu.tp_un.un_cr.un_cr_callinglen
#define	t_tpdusize	t_tpdu.tp_un.un_cr.un_cr_tpdusize
#define	t_options	t_tpdu.tp_un.un_cr.un_cr_options
#define	t_alternate	t_tpdu.tp_un.un_cr.un_cr_alternate
#define	t_cc		t_tpdu.tp_un.un_cc
#define	t_dr		t_tpdu.tp_un.un_dr
#define	t_dt		t_tpdu.tp_un.un_dt
#define	t_ed		t_tpdu.tp_un.un_ed
#define	t_er		t_tpdu.tp_un.un_er

#define	t_vdata		t_tpdu.tp_vbase
#define	t_vlen		t_tpdu.tp_vlen

#define	t_qbuf		t_tpdu.tp_qbuf

#define	t_udvec		t_tpdu.tp_udvec
};
#define	NULLPKT		((struct tsapkt *) 0)


int	freetpkt ();
struct tsapkt *newtpkt ();

void	text2tpkt (), tpkt2text ();

int	tpkt2fd ();
struct tsapkt  *fd2tpkt ();

char   *tpkt2str ();
struct tsapkt  *str2tpkt ();

/*    VARIABLE DATA codes, from ISO8073: */

					/* for CR/CC TPDUs */
#define	VDAT_TSAP_SRV	0xc2		/* TSAP ID of the calling TSAP */
#define	VDAT_TSAP_CLI	0xc1		/* TSAP ID of the called TSAP */
#define	VDAT_SIZE	0xc0		/* TPDU SIZE */
#define	  SIZE_8K	0x0d		/*   8192 */
#define	  SIZE_4K	0x0c		/*   4096 */
#define	  SIZE_2K	0x0b		/*   2048 */
#define	  SIZE_1K	0x0a		/*   1024 */
#define	  SIZE_512	0x09		/*    512 */
#define	  SIZE_256	0x08		/*    256 */
#define	  SIZE_128	0x07		/*    128 */
#define	  SIZE_DFLT	SIZE_128
#define	  SIZE_MAXTP0	SIZE_2K
#define	VDAT_VRSN	0xc4		/* Version number */
#define	VDAT_SECURITY	0xc5		/* Security parameters */
#define	VDAT_CHECKSUM	0xc3		/* Checksum */
#define	VDAT_OPTIONS	0xc6		/* Additional option selections */
#define	  OPT_NEXPEDITE	0x08		/*     Use network expedited */
#define	  OPT_CONFIRM	0x04		/*     Use receipt confirmation */
#define	  OPT_CHECKSUM	0x02		/*     Use 16-bit checksum */
#define	  OPT_TEXPEDITE	0x01		/*     Use transport expedited */
#define	VDAT_ALTERNATE	0xc7		/* Alterated protocol class(es) */
#define	  ALT_TP0	  0x01		/*     class 0 */
#define	  ALT_TP1	  0x02		/*     class 1 */
#define	  ALT_TP2	  0x04		/*     class 2 */
#define	  ALT_TP3	  0x08		/*     class 3 */
#define	  ALT_TP4	  0x10		/*     class 4 */
#define	VDAT_ACKTIME	0x85		/* Acknowledge time */
#define	VDAT_THROUGHPUT	0x89		/* Throughput */
#define	VDAT_ERRORATE	0x86		/* Residual error rate */
#define	VDAT_PRIORITY	0x87		/* Priority */
#define	VDAT_DELAY	0x88		/* Transit delay */
#define	VDAT_TTR	0x8b		/* Reassignment time */

					/* for DR TPDUs */
#define	VDAT_ADDITIONAL	0xe0		/* Additional information */

					/* for AK TPDUs */
#define	VDAT_SUBSEQ	0x8c		/* Sub-sequence number */
#define	VDAT_FLOWCTL	0x8b		/* Flow control confirmation */

					/* for ER TPDUs */
#define	VDAT_INVALID	0xc1		/* invalid TPDU */

/*  */

/* TP0 is the protocol */

int	tp0init ();
int	tp0write ();


/* TCP is NS-provider */

int	tcpopen ();

char   *tcpsave ();
int	tcprestore ();


/* X.25 is NS-provider */

int	x25open ();

char   *x25save ();
int	x25restore ();


/* Bridge is NS-provider */

int	bridgeopen ();
int	bridgediscrim ();

char	*bridgesave ();
int	bridgerestore ();


/* TP4 is the protocol and the TS-provider */

int	tp4init ();

int	tp4open ();

char   *tp4save ();
int	tp4restore ();
