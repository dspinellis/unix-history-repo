/* ssap.h - include file for session users (SS-USER) */

/* 
 * $Header: /f/osi/h/RCS/ssap.h,v 7.2 91/02/22 09:25:09 mrose Interim $
 *
 *
 * $Log:	ssap.h,v $
 * Revision 7.2  91/02/22  09:25:09  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/10  04:11:13  mrose
 * foo
 * 
 * Revision 7.0  89/11/23  21:56:02  mrose
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


#ifndef	_SSAP_
#define	_SSAP_

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

struct SSAPactid {		/* Activity Identifier */
#define	SID_DATA_SIZE	6
    u_char  sd_len;
    char    sd_data[SID_DATA_SIZE];
};

struct SSAPref {		/* SSAP reference */
				/* USER reference */
#define	SREF_USER_SIZE		64
    u_char  sr_ulen;
    char    sr_udata[SREF_USER_SIZE];

				/* COMMON reference */
#define	SREF_COMM_SIZE		64
    u_char  sr_clen;
    char    sr_cdata[SREF_COMM_SIZE];

				/* ADDITIONAL reference */
#define	SREF_ADDT_SIZE	4
    u_char  sr_alen;
    char    sr_adata[SREF_ADDT_SIZE];

				/* for S-ACTIVITY-RESUME */
    u_char  sr_vlen;
    char    sr_vdata[SREF_USER_SIZE];

#define	sr_calling	sr_udata
#define	sr_calling_len	sr_ulen
#define	sr_called	sr_vdata
#define	sr_called_len	sr_vlen
};

/*  */

struct SSAPstart {		/* S-CONNECT.INDICATION */
    int	    ss_sd;		/* SESSION descriptor */

    struct SSAPref  ss_connect;	/* session connection identifier */
    
    struct SSAPaddr ss_calling;	/* address of peer calling */
    struct SSAPaddr ss_called;	/* address of peer called */

    int	    ss_requirements;	/* session requirements */
    int	    ss_settings;	/* initial settings of tokens */
    long    ss_isn;		/* initial serial number */

    int	    ss_ssdusize;	/* largest atomic SSDU */

    struct QOStype ss_qos;	/* quality of service */

				/* initial DATA from peer */
#define	SS_SIZE		512
    int	    ss_cc;		/*   length */
    char   *ss_data;		/*   data */
};
#define	SSFREE(ss) \
{ \
    if ((ss) -> ss_data) \
	free ((ss) -> ss_data), (ss) -> ss_data = NULL; \
}

    
struct SSAPconnect {		/* S-CONNECT.CONFIRMATION */
    int	    sc_sd;		/* SESSION descriptor */

    struct SSAPref  sc_connect;	/* session connection identifier */
    
    struct SSAPaddr sc_responding;/* address of peer responding */

    int	    sc_result;		/* result */
#define	SC_ACCEPT	(-1)

#define	SC_BASE		0x80		/* reject by SSAP-provider */
#define	SC_SSAPID	(SC_BASE + 1)	/* SSAP identifier unknown */
#define	SC_SSUSER	(SC_BASE + 2)	/* SS-user not attached to SSAP */
#define	SC_CONGEST	(SC_BASE + 3)	/* Congestion at SSAP */
#define	SC_VERSION	(SC_BASE + 4)	/* Proposed protocol versions not
					   supported */

					/* begin UNOFFICIAL */
#define	SC_ADDRESS	(SC_BASE + 5)	/* Address unknown */
#define	SC_REFUSED	(SC_BASE + 6)   /* Connect request refused on this
				           network connection */
#define	SC_TRANSPORT	(SC_BASE + 7)	/* Transport disconnect */
#define	SC_ABORT	(SC_BASE + 8)	/* Provider-initiated abort */
#define	SC_PROTOCOL	(SC_BASE + 9)	/* Protocol error */
#define	SC_PARAMETER	(SC_BASE + 10)	/* Invalid parameter */
#define	SC_OPERATION	(SC_BASE + 11)	/* Invalid operation */
#define	SC_TIMER	(SC_BASE + 12)	/* Timer expired */
#define	SC_WAITING	(SC_BASE + 13)	/* Indications waiting */
					/* end UNOFFICIAL */

					/* reject by SSAP-user */
#define	SC_NOTSPECIFIED	0		/* Reason not specified */
#define	SC_CONGESTION	1		/* Temporary congestion */
#define	SC_REJECTED	2		/* Rejected */

#ifdef	notdef
#define	SC_FATAL(r)	((r) < SC_BASE || (r) < SC_PARAMETER)
#define	SC_OFFICIAL(r)	((r) < SC_BASE || (r) < SC_ADDRESS)
#else
#define	SC_FATAL(r)	((r) < SC_PARAMETER)
#define	SC_OFFICIAL(r)	((r) < SC_ADDRESS)
#endif

    int	    sc_requirements;	/* session requirements */
    int	    sc_settings;	/* initial assignment of tokens */
    int	    sc_please;		/* tokens requested by SS-user
				   (S-TOKEN-PLEASE.INDICATION) */
    long    sc_isn;		/* initial serial number */

    int	    sc_ssdusize;	/* largest atomic SSDU */

    struct QOStype sc_qos;	/* quality of service */

				/* initial DATA from peer */
#ifdef	HPUX
#undef	SC_SIZE
#endif
#define	SC_SIZE		512
    int	    sc_cc;		/*   length */
    char   *sc_data;		/*   data */
    char   *sc_realdata;	/*   real head of data */
};
#define	SCFREE(sc) \
{ \
    if ((sc) -> sc_realdata) \
	free ((sc) -> sc_realdata), \
	    (sc) -> sc_realdata = (sc) -> sc_data = NULL; \
    else \
	if ((sc) -> sc_data) \
	    free ((sc) -> sc_data), (sc) -> sc_data = NULL; \
}


					/* SESSION requirements */
#define	SR_HALFDUPLEX	0x0001	/* half-duplex */
#define	SR_DUPLEX	0x0002	/* full-duplex */
#define	SR_EXPEDITED	0x0004	/* expedited data transfer */
#define	SR_MINORSYNC	0x0008	/* minor synchronize */
#define	SR_MAJORSYNC	0x0010	/* major synchronize */
#define	SR_RESYNC	0x0020	/* resynchronize */
#define	SR_ACTIVITY	0x0040	/* activity management */
#define	SR_NEGOTIATED	0x0080	/* negotiated release */
#define	SR_CAPABILITY	0x0100	/* capability data transfer */
#define	SR_EXCEPTIONS	0x0200	/* exceptions reporting */
#define	SR_TYPEDATA	0x0400	/* typed data transfer */
#define	SR_SYMMETRIC	0x0800	/* symmetric synchronize */

#define	SR_RLS_EXISTS	SR_NEGOTIATED
#define	SR_MAJ_EXISTS	(SR_MAJORSYNC | SR_ACTIVITY)
#define	SR_ACT_EXISTS	SR_ACTIVITY
#define	SR_MIN_EXISTS	SR_MINORSYNC
#define	SR_DAT_EXISTS	SR_HALFDUPLEX

#define	SR_TOKENS	(SR_RLS_EXISTS | SR_MAJ_EXISTS | SR_ACT_EXISTS \
			    | SR_MIN_EXISTS | SR_DAT_EXISTS)

#define	SR_BASUBSET	(SR_HALFDUPLEX | SR_TYPEDATA | SR_CAPABILITY \
			    | SR_MINORSYNC | SR_EXCEPTIONS | SR_ACTIVITY)
#define	SR_BCSUBSET	(SR_HALFDUPLEX | SR_DUPLEX)
#define	SR_BSSUBSET	(SR_NEGOTIATED | SR_HALFDUPLEX | SR_DUPLEX \
			    | SR_TYPEDATA | SR_MINORSYNC | SR_MAJORSYNC \
			    | SR_RESYNC)
#define	SR_MYREQUIRE	(SR_BASUBSET | SR_BCSUBSET | SR_BSSUBSET \
			    | SR_EXPEDITED)
#define	SR_DEFAULT	(SR_HALFDUPLEX | SR_MINORSYNC | SR_ACTIVITY \
			    | SR_CAPABILITY | SR_EXCEPTIONS)

					/* SESSION tokens */
#define	ST_INIT_VALUE	0x00	/* initiator's side */
#define	ST_RESP_VALUE	0x01	/* responder's side */
#define	ST_CALL_VALUE	0x02	/* called SS-user's choice */
#define	ST_RSVD_VALUE	0x03	/* reserved */

#define	ST_MASK		0x03

#define	ST_RLS_SHIFT	6	/* release token */
#define	ST_MAJ_SHIFT	4	/* major/activity token */
#define	ST_ACT_SHIFT	ST_MAJ_SHIFT
#define	ST_MIN_SHIFT	2	/* synchronize-minor token */
#define	ST_DAT_SHIFT	0	/* data token */

#define	dotokens() \
{ \
    dotoken (SR_RLS_EXISTS, ST_RLS_SHIFT, ST_RLS_TOKEN, "release"); \
    dotoken (SR_MAJ_EXISTS, ST_MAJ_SHIFT, ST_MAJ_TOKEN, "majorsync"); \
    dotoken (SR_MIN_EXISTS, ST_MIN_SHIFT, ST_MIN_TOKEN, "minorsync"); \
    dotoken (SR_DAT_EXISTS, ST_DAT_SHIFT, ST_DAT_TOKEN, "data"); \
}


struct SSAPdata {		/* S-READ.INDICATION */
    int	    sx_type;		/* type of indication */
#define	SX_NORMAL	0x00	/* S-DATA.INDICATION */
#define	SX_EXPEDITED	0x01	/* S-EXPEDITED-DATA.INDICATION */
#define	SX_TYPED	0x02	/* S-TYPED-DATA.INDICATION */
#define	SX_CAPDIND	0x03	/* S-CAPABILITY-DATA.INDICATION */
#define	SX_CAPDCNF	0x04	/* S-CAPABILITY-DATA.CONFIRMATION */

#define	SX_EXSIZE	14	/* EXPEDITED DATA (XSSDU) only */
#define	SX_CDSIZE	512	/* CAPABILITY DATA only */
#define	SX_CDASIZE	512	/* CAPABILITY DATA ACK only */
    int	    sx_cc;		/*   total length */
    struct qbuf sx_qbuf;	/*   chained data */
};
#define	SXFREE(sx)	QBFREE (&((sx) -> sx_qbuf))


struct SSAPtoken {		/* S-{TOKEN-*,GIVE-CONTROL}.INDICATION */
    int	    st_type;		/* type of indication */
#define	ST_GIVE		0x00	/* S-TOKEN-GIVE.INDICATION */
#define	ST_PLEASE	0x01	/* S-TOKEN-PLEASE.INDICATION */
#define	ST_CONTROL	0x02	/* S-GIVE-CONTROL.INDICATION */

    u_char  st_tokens;		/* tokens offered/wanted */
#define	ST_RLS_TOKEN	(0x01 << ST_RLS_SHIFT)
#define	ST_MAJ_TOKEN	(0x01 << ST_MAJ_SHIFT)
#define	ST_ACT_TOKEN	(0x01 << ST_ACT_SHIFT)
#define	ST_MIN_TOKEN	(0x01 << ST_MIN_SHIFT)
#define	ST_DAT_TOKEN	(0x01 << ST_DAT_SHIFT)

    u_char  st_owned;		/* tokens owned by user */

#define	ST_SIZE		512	/* PLEASE TOKEN only */
    int	    st_cc;		/*   length */
    char   *st_data;		/*   data */
};
#define	STFREE(st) \
{ \
    if ((st) -> st_data) \
	free ((st) -> st_data), (st) -> st_data = NULL; \
}


struct SSAPsync {		/* S-*-SYNC.{INDICATION,CONFIRMATION} */
    int	    sn_type;		/* type of indication/confirmation */
#define	SN_MAJORIND	0x00	/* S-MAJOR-SYNC.INDICATION */
#define	SN_MAJORCNF	0x01	/* S-MAJOR-SYNC.CONFIRMATION */
#define	SN_MINORIND	0x02	/* S-MINOR-SYNC.INDICATION */
#define	SN_MINORCNF	0x03	/* S-MINOR-SYNC.CONFIRMATION */
#define	SN_RESETIND	0x04	/* S-RESYNCHRONIZE.INDICATION */
#define	SN_RESETCNF	0x05	/* S-RESYNCHRONIZE.CONFIRMATION */

    int	    sn_options;		/* options (!!) */
				/* for S-MINOR-SYNC.INDICATION */
#define	SYNC_CONFIRM	1	/* wants confirmation */
#define	SYNC_NOCONFIRM	0	/*   .. nope */
				/* for S-RESYNCHRONIZE.INDICATION */
#define	SYNC_RESTART	0	/* restart */
#define	SYNC_ABANDON	1	/* abandon */
#define	SYNC_SET	2	/* set */

    long    sn_ssn;		/* serial number */
#define	SERIAL_NONE	(-1L)	/* No SSN */
#define	SERIAL_MIN	000000L	/* the min SSN */
#define	SERIAL_MAX	999998L	/* the max SSN */

    int	    sn_settings;	/* new token settings (RESYNC only) */

				/* sync data from peer */
#define	SN_SIZE		512
    int	    sn_cc;		/*   length */
    char   *sn_data;		/*   data */
};
#define	SNFREE(sn) \
{ \
    if ((sn) -> sn_data) \
	free ((sn) -> sn_data), (sn) -> sn_data = NULL; \
}


struct SSAPactivity {		/* S-ACTIVITY-*.{INDICATION,CONFIRMATION} */
    int	    sv_type;		/* type of indication/confirmation */
#define	SV_START	0x00	/* S-ACTIVITY-START.INDICATION */
#define	SV_RESUME	0x01	/* S-ACTIVITY-RESUME.INDICATION */
#define	SV_INTRIND	0x02	/* S-ACTIVITY-INTERRUPT.INDICATION */
#define	SV_INTRCNF	0x03	/* S-ACTIVITY-INTERRUPT.CONFIRMATION */
#define	SV_DISCIND	0x04	/* S-ACTIVITY-DISCARD.INDICATION */
#define	SV_DISCCNF	0x05	/* S-ACTIVITY-DISCARD.CONFIRMATION */
#define	SV_ENDIND	0x06	/* S-ACTIVITY-END.INDICATION */
#define	SV_ENDCNF	0x07	/* S-ACTIVITY-END.CONFIRMATION */

    struct SSAPactid sv_id;	/* START/RESUME activity identifier */

    struct SSAPactid sv_oid;	/* RESUME old activity identifier */
    struct SSAPref   sv_connect;/* 	  old connection identifier */

    long    sv_ssn;		/* RESUME/END Serial number */

    int	    sv_reason;		/* INTERRUPT/DISCARD */
				/* same values as sp_reason */
        
				/* activity DATA from peer */
#define	SV_SIZE		512
    int	    sv_cc;		/*   length */
    char   *sv_data;		/*   data */
};
#define	SVFREE(sv) \
{ \
    if ((sv) -> sv_data) \
	free ((sv) -> sv_data), (sv) -> sv_data = NULL; \
}


struct SSAPreport {		/* S-{U,P}-EXCEPTION-REPORT.INDICATION */
    int	    sp_peer;		/* T   = S-U-EXCEPTION-REPORT.INDICATION:
					sp_reason/sp_data both meaningful
				   NIL = S-P-EXCEPTION-REPORT.INDICATION:
					sp_reason == SP_NOREASON, or
					sp_reason == SP_PROTOCOL */
    int	    sp_reason;
#define	SP_NOREASON	0	/* No specific reason stated */
#define	SP_JEOPARDY	1	/* User receiving ability jeopardized */
#define	SP_RSVD1	2	/* reserved */
#define	SP_SEQUENCE	3	/* User sequence error */
#define	SP_RSVD2	4	/* reserved */
#define	SP_LOCAL	5	/* Local SS-user error */
#define	SP_PROCEDURAL	6	/* Unrecoverable procedural error */
#define	SP_DEMAND	128	/* Demand data token */

#define	SP_PROTOCOL	(-1)	/* SS-provider protocol error */

				/* report DATA from peer */
#define	SP_SIZE		512
    int	    sp_cc;		/*   length */
    char   *sp_data;		/*   data */
};
#define	SPFREE(sp) \
{ \
    if ((sp) -> sp_data) \
	free ((sp) -> sp_data), (sp) -> sp_data = NULL; \
}


struct SSAPfinish {		/* S-RELEASE.INDICATION */
				/* release DATA from peer */
#define	SF_SIZE		512
    int	    sf_cc;		/*   length */
    char   *sf_data;		/*   data */
};
#define	SFFREE(sf) \
{ \
    if ((sf) -> sf_data) \
	free ((sf) -> sf_data), (sf) -> sf_data = NULL; \
}


struct SSAPrelease {		/* S-RELEASE.CONFIRMATION */
    int	    sr_affirmative;	/* T   = connection released
				   NIL = request refused */

				/* release DATA from peer */
#define	SR_SIZE		512
    int	    sr_cc;		/*   length */
    char   *sr_data;		/*   data */
};
#define	SRFREE(sr) \
{ \
    if ((sr) -> sr_data) \
	free ((sr) -> sr_data), (sr) -> sr_data = NULL; \
}


struct SSAPabort {		/* S-{U,P}-ABORT.INDICATION */
    int	    sa_peer;		/* T   = S-U-ABORT.INDICATION:
					     sa_info/sa_cc is meaningful
				   NIL = S-P-ABORT.INDICATION:
					     sa_reason is meaningful,
					     sa_data/sa_cc contains diagnostics */

    int	    sa_reason;		/* same codes as sc_result */

				/* abort DATA from peer */
#define	SA_SIZE		512	/* N.B.: the ISO DIS says 9, but we use
				   512 instead so ASE-level aborts will work
				   reasonably */
    int	    sa_cc;		/*   length */
    char   *sa_info;		/*   data (from the peer) */
    char   *sa_realinfo;	/*   real head of data */
    char    sa_data[512];	/*   data (for messages from provider) */
};
#define	SAFREE(sa) \
{ \
    if ((sa) -> sa_realinfo) \
	free ((sa) -> sa_realinfo), (sa) -> sa_realinfo = NULL; \
}


struct SSAPindication {
    int	    si_type;		/* the union element present */
#define	SI_DATA		0x00
#define	SI_TOKEN	0x01
#define	SI_SYNC		0x02
#define	SI_ACTIVITY	0x03
#define	SI_REPORT	0x04
#define	SI_FINISH	0x05
#define	SI_ABORT	0x06

    union {
	struct SSAPdata si_un_data;
	struct SSAPtoken si_un_token;
	struct SSAPsync si_un_sync;
	struct SSAPactivity si_un_activity;
	struct SSAPreport si_un_report;
	struct SSAPfinish si_un_finish;
	struct SSAPabort si_un_abort;
    }	si_un;
#define	si_data		si_un.si_un_data
#define	si_token	si_un.si_un_token
#define	si_sync		si_un.si_un_sync
#define	si_activity	si_un.si_un_activity
#define	si_report	si_un.si_un_report
#define	si_finish	si_un.si_un_finish
#define	si_abort	si_un.si_un_abort
};

/*  */

extern char *ssapversion;


int	SExec ();		/* SERVER only */
int	SInit ();		/* S-CONNECT.INDICATION */

int	SConnResponse ();	/* S-CONNECT.RESPONSE */
int	SConnRequest ();	/* S-CONNECT.REQUEST (backwards-compatible) */
#define	SConnRequest(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) \
	SAsynConnRequest (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,0)
int	SAsynConnRequest ();	/* S-(ASYN-)CONNECT.REQUEST */
int	SAsynRetryRequest ();	/* S-ASYN-RETRY.REQUEST (pseudo) */
int	SDataRequest ();	/* S-DATA.REQUEST */
int	SSendRequest ();	/* S-SEND.REQUEST (segmented) */
int	SWriteRequest ();	/* S-WRITE.REQUEST (pseudo) */
int	SExpdRequest ();	/* S-EXPEDITED-DATA.REQUEST */
int	STypedRequest ();	/* S-TYPED-DATA.REQUEST */
int	SCapdRequest ();	/* S-CAPABILITY-DATA.REQUEST */
int	SCapdResponse ();	/* S-CAPABILITY-DATA.RESPONSE */
int	SReadRequest ();	/* S-READ.REQUEST (pseudo) */
int	SGTokenRequest ();	/* S-TOKEN-GIVE.REQUEST */
int	SPTokenRequest ();	/* S-TOKEN-PLEASE.REQUEST */
int	SGControlRequest ();	/* S-CONTROL-GIVE.REQUEST */
int	SMajSyncRequest ();	/* S-MAJOR-SYNC.REQUEST */
int	SMajSyncResponse ();	/* S-MAJOR-SYNC.RESPONSE */
int	SMinSyncRequest ();	/* S-MINOR-SYNC.REQUEST */
int	SMinSyncResponse ();	/* S-MINOR-SYNC.RESPONSE */
int	SReSyncRequest ();	/* S-RESYNCHRONIZE.REQUEST */
int	SReSyncResponse ();	/* S-RESYNCHRONIZE.RESPONSE */
int	SActStartRequest ();	/* S-ACTIVITY-START.REQUEST */
int	SActResumeRequest ();	/* S-ACTIVITY-RESUME.REQUEST */
int	SActIntrRequest ();	/* S-ACTIVITY-INTERRUPT.REQUEST */
int	SActIntrResponse ();	/* S-ACTIVITY-INTERRUPT.RESPONSE */
int	SActDiscRequest ();	/* S-ACTIVITY-DISCARD.REQUEST */
int	SActDiscResponse ();	/* S-ACTIVITY-DISCARD.RESPONSE */
int	SActEndRequest ();	/* S-ACTIVITY-END.REQUEST */
int	SActEndResponse ();	/* S-ACTIVITY-END.RESPONSE */
int	SUAbortRequest ();	/* S-U-ABORT.REQUEST */
int	SUReportRequest ();	/* S-U-EXCEPTION-REPORT.REQUEST */
int	SRelRequest ();		/* S-RELEASE.REQUEST */
int	SRelRetryRequest ();	/* S-RELEASE-RETRY.REQUEST (pseudo) */
int	SRelResponse ();	/* S-RELEASE.RESPONSE */

int	SSetIndications ();	/* define vectors for INDICATION events */
int	SSelectMask ();		/* map session descriptors for select() */

char   *SErrString ();		/* return SSAP error code in string form */

#define	SLocalHostName	getlocalhost
char   *SLocalHostName ();	/* return name of local host (sigh) */
#endif
