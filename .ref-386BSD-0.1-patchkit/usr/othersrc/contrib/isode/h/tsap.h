/* tsap.h - include file for transport users (TS-USER) */

/* 
 * $Header: /f/osi/h/RCS/tsap.h,v 7.4 91/02/22 09:25:16 mrose Interim $
 *
 *
 * $Log:	tsap.h,v $
 * Revision 7.4  91/02/22  09:25:16  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/11/21  11:32:15  mrose
 * sun
 * 
 * Revision 7.2  90/05/08  08:54:36  mrose
 * touch-up
 * 
 * Revision 7.1  89/12/18  17:50:50  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:56:08  mrose
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


#ifndef	_TSAP_
#define	_TSAP_			

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif
#ifndef	_ISOADDRS_
#include "isoaddrs.h"
#endif

/*  */

struct TSAPstart {		/* T-CONNECT.INDICATON */
    int     ts_sd;		/* TRANSPORT descriptor */

    struct TSAPaddr ts_calling;	/* address of peer calling */
    struct TSAPaddr ts_called;	/* address of peer called */

    int     ts_expedited;	/* EXPEDITED DATA ok */

    int	    ts_tsdusize;	/* largest atomic TSDU */

    struct QOStype ts_qos;	/* quality of service */

				/* initial DATA from peer */
#define	TS_SIZE		32
    int	    ts_cc;		/*   length */
    char    ts_data[TS_SIZE];	/*   data */
};


struct TSAPconnect {		/* T-CONNECT.CONFIRMATION */
    int     tc_sd;		/* TRANSPORT descriptor */

    struct TSAPaddr tc_responding;/* address of peer responding */

    int     tc_expedited;	/* EXPEDITED DATA ok */

    int	    tc_tsdusize;	/* largest atomic TSDU */

    struct QOStype tc_qos;	/* quality of service */

				/* initial DATA from peer */
#define	TC_SIZE		32
    int	    tc_cc;		/*   length */
    char    tc_data[TC_SIZE];	/*   data */
};


struct TSAPdata {		/* T-READ.INDICATION */
    int     tx_expedited;

				/* DATA from peer */
#define	TX_SIZE		16	/* EXPEDITED DATA only */
    int	    tx_cc;		/*   total length */
    struct qbuf tx_qbuf;	/*   chained data */
};
#define	TXFREE(tx)	QBFREE (&((tx) -> tx_qbuf))


struct TSAPdisconnect {		/* T-DISCONNECT.INDICATION */
    int     td_reason;		/* reason for DISCONNECT, from ISO8072: */
#define	DR_BASE		0x80
#define	DR_NORMAL	(DR_BASE + 0)	/* NORMAL disconnect by SESSION
					   entity */
#define	DR_REMOTE	(DR_BASE + 1)	/* Remote TRANSPORT entity congested at
					   connect request time */
#define	DR_CONNECT	(DR_BASE + 2)	/* Connection negotiation failed */
#define	DR_DUPLICATE	(DR_BASE + 3)	/* Duplicate source reference detected
				           for the same pair of NSAPs */
#define	DR_MISMATCH	(DR_BASE + 4)	/* Mismatched references */
#define	DR_PROTOCOL	(DR_BASE + 5)	/* Protocol error */
#define	DR_OVERFLOW	(DR_BASE + 7)	/* Reference overflow */
#define	DR_REFUSED	(DR_BASE + 8)	/* Connect request refused on this
				           network connection */
#define	DR_LENGTH	(DR_BASE + 10)	/* Header or parameter length
					   invalid */

					/* begin UNOFFICIAL */
#define	DR_NETWORK	(DR_BASE + 11)	/* Network disconnect */
#define	DR_PARAMETER	(DR_BASE + 12)	/* Invalid parameter */
#define	DR_OPERATION	(DR_BASE + 13)	/* Invalid operation */
#define	DR_TIMER	(DR_BASE + 14)	/* Timer expired */
#define	DR_WAITING	(DR_BASE + 15)	/* Indications waiting */
					/* end UNOFFICIAL */

#define	DR_UNKNOWN	0		/* Reason not specifed */
#define	DR_CONGEST	1		/* Congestion at TSAP */
#define	DR_SESSION	2		/* Session entity not attached to
					   TSAP */
#define	DR_ADDRESS	3		/* Address unknown */

#ifdef	notdef
#define	DR_FATAL(r)	((r) < DR_BASE || (r) < DR_PARAMETER)
#define	DR_OFFICIAL(r)	((r) < DR_BASE || (r) < DR_NETWORK)
#else
#define	DR_FATAL(r)	((r) < DR_PARAMETER)
#define	DR_OFFICIAL(r)	((r) < DR_NETWORK)
#endif

#define DR_CONS			256		/* Base for CONS reson codes */
#define DR_CONS_UNDEFINED  	DR_CONS+0	/* Undefined */
		/* originator: NS_PROVIDER */
#define	DR_CONS_PROVIDER	DR_CONS+224	/* GENERIC */
#define	DR_CP_DIS_TRANS		DR_CONS+225	/* Disconnect - transient */
#define	DR_CP_DIS_PERM		DR_CONS+226	/* Disconnect - permanent */
#define	DR_CP_REJ_UNSPEC_TRANS	DR_CONS+227	/* Reject - transient */
#define	DR_CP_REJ_UNSPEC_PERM  	DR_CONS+228	/* Reject - permanent */
#define	DR_CP_REJ_NO_QOS_TRANS 	DR_CONS+229	/* No QOS - transient */
#define	DR_CP_REJ_NO_QOS_PERM  	DR_CONS+230	/* No QOS - permanent */
#define	DR_CP_REJ_NSAP_UNREACH_TRANS 	DR_CONS+231	/* NSAP unreachable */
#define	DR_CP_REJ_NSAP_UNREACH_PERM  	DR_CONS+232	/* NSAP unreachable */
#define	DR_CP_RESET_UNSPEC	DR_CONS+233	/* Unspecified RESET */
#define	DR_CP_RESET_CONGESTION 	DR_CONS+234	/* RESET due to congestion */
#define	DR_CP_REJ_NSAP_UNKNOWN_PERM  	DR_CONS+235	/* Unknown NSAP */
		/* originator: NS_USER */
#define	DR_CONS_USER		DR_CONS+240	/* GENERIC */
#define	DR_CU_DIS_NORMAL	DR_CONS+241	/* Normal disconnect */
#define	DR_CU_DIS_ABNORMAL	DR_CONS+242	/* Abnormal disconnect */
#define	DR_CU_REJ_UNSPEC_TRANS	DR_CONS+244	/* Reject - transient */
#define	DR_CU_REJ_UNSPEC_PERM	DR_CONS+245	/* Reject - permanent */
#define	DR_CU_REJ_NO_QOS_TRANS 	DR_CONS+246	/* No QOS - transient */
#define	DR_CU_REJ_NO_QOS_PERM  	DR_CONS+247	/* No QOS - permanent */
#define	DR_CU_REJ_INCOMPAT	DR_CONS+248	/* Incompatable NS user data */
#define	DR_CU_RESET_USER_RESYNCH DR_CONS+250	/* User RESET */

				/* disconnect DATA from peer */
#define	TD_SIZE		64
    int	    td_cc;		/*   length */
    char    td_data[TD_SIZE];	/*   data */
};

/*  */

extern char *tsapversion;


int	TInit ();		/* T-CONNECT.INDICATION */

int	TConnResponse ();	/* T-CONNECT.RESPONSE */
				/* T-CONNECT.REQUEST (backwards-compatible) */
#define	TConnRequest(a1,a2,a3,a4,a5,a6,a7,a8) \
	TAsynConnRequest(a1,a2,a3,a4,a5,a6,a7,a8,0)
int	TAsynConnRequest ();	/* T-(ASYN-)CONNECT.REQUEST */
int	TAsynRetryRequest ();	/* T-ASYN-RETRY.REQUEST (pseudo) */
int	TDataRequest ();	/* T-DATA.REQUEST */
int	TWriteRequest ();	/* T-WRITE.REQUEST (pseudo) */
int	TExpdRequest ();	/* T-EXPEDITED-DATA.REQUEST */
int	TReadRequest ();	/* T-READ.REQUEST (pseudo) */
int	TDiscRequest ();	/* T-DISCONNECT.REQUEST */

int	TSetIndications ();	/* define vectors for INDICATION events */
int	TSelectMask ();		/* map transport descriptors for select() */
int	TSelectOctets ();	/* estimate of octets that might be returned */
int	TGetAddresses ();	/* get TSAPs */
int	TSetManager ();		/* defining transport manager */

char   *TErrString ();		/* return TSAP error code in string form */

int	TNetListen ();		/* start listenting on an TSAP */
int	TNetUnique ();		/* start listenting on a set of unique TSAPs */
#define	TNetAccept(p,v,n,r,w,e,s,t) \
	TNetAcceptAux ((p), (v), NULLIP, NULLTA, (n), (r), (w), (e), (s), (t))
int	TNetAcceptAux ();	/* accept a call on an TSAP */
int	TNetClose ();		/* stop listening on an TSAP */
int	TSetQueuesOK ();	/* enable/disable queued (non-blocking)
				   writes */

#define	TLocalHostName	getlocalhost
char   *TLocalHostName ();	/* return name of local host (sigh) */
#endif
