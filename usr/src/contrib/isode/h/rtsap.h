/* rtsap.h - include file for reliable transfer users (RtS-USER) */

/* 
 * $Header: /f/osi/h/RCS/rtsap.h,v 7.3 91/02/22 09:25:05 mrose Interim $
 *
 *
 * $Log:	rtsap.h,v $
 * Revision 7.3  91/02/22  09:25:05  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/23  20:35:53  mrose
 * update
 * 
 * Revision 7.1  90/07/27  08:44:53  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:55:59  mrose
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


#ifndef	_RtSAP_
#define	_RtSAP_

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif

#ifndef	_AcSAP_
#include "acsap.h"		/* definitions for AcS-USERs */
#endif

/*  */

struct RtSAPaddr {		/* RtSAP address (old-style) */
    struct SSAPaddr rta_addr;

    u_short	rta_port;
};
#define	NULLRtA			((struct RtSAPaddr *) 0)

/*  */

struct RtSAPstart {		/* RT-OPEN.INDICATION */
    int	    rts_sd;		/* RTS descriptor */

    struct RtSAPaddr rts_initiator;/* address */

    int	    rts_mode;		/* dialogue mode */
#define	RTS_MONOLOGUE	0	/*   monologue */
#define	RTS_TWA		1	/*   two-way alternate */

    int	    rts_turn;		/* initial turn */
#define	RTS_INITIATOR	0	/*   the initiator */
#define	RTS_RESPONDER	1	/*   the responder */

    u_short rts_port;		/* application number */

    PE	    rts_data;		/* initial DATA from peer */

    struct AcSAPstart rts_start;/* info from A-ASSOCIATE.INDICATION */
};
#define	RTSFREE(rts) \
{ \
    if ((rts) -> rts_data) \
	pe_free ((rts) -> rts_data), (rts) -> rts_data = NULLPE; \
    ACSFREE (&((rts) -> rts_start)); \
}


struct RtSAPconnect {		/* RT-OPEN.CONFIRMATION */
    int	    rtc_sd;		/* RTS descriptor */

    int	    rtc_result;		/* disposition */
#define	RTS_ACCEPT	(-1)

				/* RT-OPEN.REQUEST */
#define	RTS_BUSY	0	/* Busy */
#define	RTS_RECOVER	1	/* Cannot recover */
#define	RTS_VALIDATE	2	/* Validation failure */
#define	RTS_MODE	3	/* Unacceptable dialogue mode */
#define	RTS_REJECT	4	/* Rejected by responder */

    PE	    rtc_data;		/* initial DATA from peer */

    struct AcSAPconnect rtc_connect;/* info from A-ASSOCIATE.CONFIRMATION */
};
#define	RTCFREE(rtc) \
{ \
    if ((rtc) -> rtc_data) \
	pe_free ((rtc) -> rtc_data), (rtc) -> rtc_data = NULLPE; \
    ACCFREE (&((rtc) -> rtc_connect)); \
}


struct RtSAPturn {		/* RT-TURN-{GIVE,PLEASE}.INDICATION */
    int	    rtu_please;		/* T   = RT-TURN-PLEASE.INDICATION
				   NIL = RT-TURN-GIVE.INDICATION */

    int	    rtu_priority;	/* priority, iff rtu_please == T */
};


struct RtSAPtransfer {		/* RT-TRANSFER.INDICATION */
    PE	    rtt_data;
};
#define	RTTFREE(rtt) \
{ \
    if ((rtt) -> rtt_data) \
	pe_free ((rtt) -> rtt_data), (rtt) -> rtt_data = NULLPE; \
}


struct RtSAPabort {		/* RT-{U,P}-ABORT.INDICATION */
    int     rta_peer;		/* T   = RT-U-ABORT.INDICATION
				   NIL = RT-P-ABORT.INDICATION */

    int	    rta_reason;		/* reason */
				/* begin UNOFFICIAL */
#define	RTS_ADDRESS	5	/* Address unknown */
#define	RTS_REFUSED	6	/* Connect request refused on this network
				   connection */
#define	RTS_SESSION	7	/* Session disconnect */
#define	RTS_PROTOCOL	8	/* Protocol error */
#define	RTS_CONGEST	9	/* Congestion at RtSAP */
#define	RTS_REMOTE	10	/* Remote system problem */
#define	RTS_PRESENTATION 11	/* Presentation disconnect */
#define	RTS_ACS		12	/* ACS disconnect */
#define	RTS_ABORTED	13	/* Peer aborted association */
#define	RTS_PARAMETER	14	/* Invalid parameter */
#define	RTS_OPERATION	15	/* Invalid operation */
#define	RTS_TIMER	16	/* Timer expired */
#define	RTS_WAITING	17	/* Indications waiting */
#define	RTS_TRANSFER	18	/* Transfer failure (not really unofficial) */
				/* end UNOFFICIAL */

#define	RTS_FATAL(r)	((r) < RTS_PARAMETER)
#define	RTS_OFFICIAL(r)	((r) < RTS_ADDRESS)

    PE	    rta_udata;		/* failure data from user */

				/* additional failure data from provider */
#define	RTA_SIZE	512
    int	    rta_cc;		/*   length */
    char    rta_data[RTA_SIZE];	/*   data */
};
#define	RTAFREE(rta) \
{ \
    if ((rta) -> rta_udata) \
	pe_free ((rta) -> rta_udata), (rta) -> rta_udata = NULLPE; \
}


struct RtSAPclose {		/* RT-CLOSE.INDICATION */
    int	    rtc_dummy;
};


struct RtSAPindication {
    int	    rti_type;		/* the union element present */
#define	RTI_TURN	0x00
#define	RTI_TRANSFER	0x01
#define	RTI_ABORT	0x02
#define	RTI_CLOSE	0x03
#define	RTI_FINISH	0x04

    union {
	struct RtSAPturn rti_un_turn;
	struct RtSAPtransfer rti_un_transfer;
	struct RtSAPabort rti_un_abort;
	struct RtSAPclose rti_un_close;
	struct AcSAPfinish rti_un_finish;
    }	rti_un;
#define	rti_turn	rti_un.rti_un_turn
#define	rti_transfer	rti_un.rti_un_transfer
#define	rti_abort	rti_un.rti_un_abort
#define	rti_close	rti_un.rti_un_close
#define	rti_finish	rti_un.rti_un_finish
};
    
/*  */

extern char *rtsapversion;

				/* RTSE primitives */
int	RtInit_Aux ();		/* RT-OPEN.INDICATION */
#define RtInit(vecp,vec,rts,rti) \
	RtInit_Aux((vecp),(vec),(rts),(rti),NULLOID)
int	RtOpenResponse ();	/* RT-OPEN.RESPONSE */
#define RtOpenRequest(mode,turn,ctx,ct1,ct2,ca1,ca2,ctxl,defc,data,qos,rtc,rti)\
	RtOpenRequest2((mode),(turn),(ctx),(ct1),(ct2),(ca1),(ca2),\
		(ctxl),(defc),(data),(qos),NULLOID,(rtc),(rti))
int	RtOpenRequest2 ();	/* RT-OPEN.REQUEST */
int	RtCloseRequest ();	/* RT-CLOSE.REQUEST */
int	RtCloseResponse ();	/* RT-CLOSE.RESPONSE */
int	RtUAbortRequest ();	/* RT-U-ABORT.REQUEST */

				/* X.410 primitives */
int	RtExec ();		/* SERVER only */
int	RtBInit ();		/* RT-BEGIN.INDICATION (X.410 OPEN.INDICATION) */
int	RtBeginResponse ();	/* RT-BEGIN.RESPONSE (X.410 OPEN.RESPONSE) */
int	RtBeginRequest2 ();	/* RT-BEGIN.REQUEST (X.410 OPEN.REQUEST) */
#define RtBeginRequest(called, mode, turn, data, rtc, rti) \
	RtBeginRequest2((called), NULLRtA, (mode), (turn), (data), (rtc), (rti))
int	RtEndRequest ();	/* RT-END.REQUEST (X.410 CLOSE.REQUEST) */
int	RtEndResponse ();	/* RT-END.RESPONSE (X.410 CLOSE.RESPONSE) */

int	RtPTurnRequest ();	/* RT-TURN-PLEASE.REQUEST */
int	RtGTurnRequest ();	/* RT-TURN-GIVE.REQUEST */
int	RtTransferRequest ();	/* RT-TRANSFER.REQUEST */
int	RtWaitRequest ();	/* RT-WAIT.REQUEST (pseudo) */

int	RtSetIndications ();	/* define vectors for INDICATION events */
int	RtSelectMask ();	/* map reliable transfer descriptors for
				   select() */

int	RtSetDownTrans ();	/* set downtrans upcall */
int	RtSetUpTrans ();	/* set uptrans upcall */

char   *RtErrString ();		/* return RtSAP error code in string form */
#endif
