/* psap2.h - include file for presentation users (PS-USER) continued  */

/* 
 * $Header: /f/osi/h/RCS/psap2.h,v 7.2 91/02/22 09:24:56 mrose Interim $
 *
 *
 * $Log:	psap2.h,v $
 * Revision 7.2  91/02/22  09:24:56  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:32:11  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:55:53  mrose
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


#ifndef	_PSAP2_
#define	_PSAP2_

#ifndef	_PSAP_
#include "psap.h"		/* definitions for PS-USERs */
#endif
#ifndef	_SSAP_
#include "ssap.h"		/* definitions for SS-USERs */
#endif
#ifndef	_ISOADDRS_
#include "isoaddrs.h"		/* definitions of ISO addresses */
#endif

/*  */

#define	NPDATA		125	/* arbitrary */


struct PSAPcontext {		/* presentation context */
    int	    pc_id;		/* identifier */

    OID	    pc_asn;		/* abstract syntax name */

    OID	    pc_atn;		/* abstract transfer name */
				/*   NULLOID if provider should handle it */
    
    int	    pc_result;		/* same codes as pc_result below */
};

struct PSAPctxlist {		/* list of presentation contexts */
    int	    pc_nctx;		/* number of contexts */

#define	NPCTX	10		/* arbitrary */
    struct PSAPcontext pc_ctx[NPCTX];
};
#define	NULLPC	((struct PSAPctxlist *) 0)


#define	BER	"basic encoding of a single asn.1 type"


struct SSAPref *addr2ref ();	/* address to session reference */

char   *sprintref ();		/* return session reference in string form */

/*  */

struct PSAPstart {		/* P-CONNECT.INDICATION */
    int	    ps_sd;		/* PRESENTATION descriptor */


    struct PSAPaddr ps_calling;	/* address of peer calling */
    struct PSAPaddr ps_called;	/* address of peer called */

    struct PSAPctxlist ps_ctxlist;/* list of proposed contexts */

    OID	    ps_defctx;		/* name of proposed default context */
    int	    ps_defctxresult;	/* what the provider thinks about it */
    
    int	    ps_prequirements;	/* presentation requirements */

    int	    ps_srequirements;	/* session requirements */
    int	    ps_settings;	/* initial settings of tokens */
    long    ps_isn;		/* initial serial number */
    
    struct SSAPref  ps_connect;	/* session connection identifier */

    int	    ps_ssdusize;	/* largest atomic SSDU */
    
    struct QOStype ps_qos;	/* quality of service */
    
				/* initial data from peer */
    int	    ps_ninfo;		/*   number of elements */
    PE	    ps_info[NPDATA];	/*   data */
};
#define	PSFREE(ps) \
{ \
    register int PSI; \
 \
    for (PSI = (ps) -> ps_ctxlist.pc_nctx - 1; PSI >= 0; PSI--) { \
	oid_free ((ps) -> ps_ctxlist.pc_ctx[PSI].pc_asn); \
	oid_free ((ps) -> ps_ctxlist.pc_ctx[PSI].pc_atn); \
	(ps) -> ps_ctxlist.pc_ctx[PSI].pc_asn = \
	    (ps) -> ps_ctxlist.pc_ctx[PSI].pc_atn = NULLOID; \
    } \
    (ps) -> ps_ctxlist.pc_nctx = 0; \
    if ((ps) -> ps_defctx) \
	oid_free ((ps) -> ps_defctx), (ps) -> ps_defctx = NULLOID; \
    for (PSI = (ps) -> ps_ninfo - 1; PSI >= 0; PSI--) \
	if ((ps) -> ps_info[PSI]) \
	    pe_free ((ps) -> ps_info[PSI]), (ps) -> ps_info[PSI] = NULLPE; \
    (ps) -> ps_ninfo = 0; \
}


struct PSAPconnect {		/* P-CONNECT.CONFIRMATION */
    int	    pc_sd;		/* PRESENTATION descriptor */

    struct PSAPaddr pc_responding;/* address of peer responding */

    struct PSAPctxlist pc_ctxlist;/* the list of negotiated contexts */

    int	    pc_defctxresult;	/* result for proposed default context name */
    
    int	    pc_prequirements;	/* presentation requirements */

    int	    pc_srequirements;	/* session requirements */
    int	    pc_settings;	/* initial settings of tokens */
    int	    pc_please;		/* tokens requested by PS-user */
    long    pc_isn;		/* initial serial number */

    struct SSAPref pc_connect;	/* session connection identifier */

    int	    pc_ssdusize;	/* largest atomic SSDU */

    struct QOStype pc_qos;	/* quality of service */
    
    int	    pc_result;		/* result */
#define	PC_ACCEPT	(-1)

#define	PC_REJECTED	0	/* Rejected by peer */
				/* Provider-reason */
#define	PC_NOTSPECIFIED	1	/* Reason not specified */
#define	PC_CONGEST	2	/* Temporary congestion */
#define	PC_EXCEEDED	3	/* Local limit exceeded */
#define	PC_ADDRESS	4	/* Called presentation address unknown */
#define	PC_VERSION	5	/* Protocol version not supported */
#define	PC_DEFAULT	6	/* Default context not supported */
#define	PC_READABLE	7	/* User-data not readable */
#define	PC_AVAILABLE	8	/* No PSAP available */
				/* Abort-reason */
#define	PC_UNRECOGNIZED	9	/* Unrecognized PPDU */
#define	PC_UNEXPECTED	10	/* Unexpected PPDU */
#define	PC_SSPRIMITIVE	11	/* Unexpected session service primitive */
#define	PC_PPPARAM1	12	/* Unrecognized PPDU parameter */
#define	PC_PPPARAM2	13	/* Unexpected PPDU parameter */
#define	PC_INVALID	14	/* Invalid PPDU parameter value */
				/* Provider-reason */
#define	PC_ABSTRACT	15	/* Abstract syntax not supported */
#define	PC_TRANSFER	16	/* Proposed transfer syntaxes not supported */
#define	PC_DCSLIMIT	17	/* Local limit on DCS exceeded */
				/* begin UNOFFICIAL */
#define	PC_REFUSED	18	/* Connect request refused on this network
				   connection */
#define	PC_SESSION	19	/* Session disconnect */
#define	PC_PROTOCOL	20	/* Protocol error */
#define	PC_ABORTED	21	/* Peer aborted connection */
#define	PC_PARAMETER	22	/* Invalid parameter */
#define	PC_OPERATION	23	/* Invalid operation */
#define	PC_TIMER	24	/* Timer expired */
#define	PC_WAITING	25	/* Indications waiting */
				/* end UNOFFICIAL */

#define	PC_FATAL(r)	((r) < PC_PARAMETER)
#define	PC_OFFICIAL(r)	((r) < PC_REFUSED)
    
				/* initial data from peer */
    int	    pc_ninfo;		/*   number of elements */
    PE	    pc_info[NPDATA];	/*   data */
};
#define	PCFREE(pc) \
{ \
    register int PCI; \
 \
    for (PCI = (pc) -> pc_ctxlist.pc_nctx - 1; PCI >= 0; PCI--) { \
	oid_free ((pc) -> pc_ctxlist.pc_ctx[PCI].pc_asn); \
	oid_free ((pc) -> pc_ctxlist.pc_ctx[PCI].pc_atn); \
	(pc) -> pc_ctxlist.pc_ctx[PCI].pc_asn = \
	    (pc) -> pc_ctxlist.pc_ctx[PCI].pc_atn = NULLOID; \
    } \
    (pc) -> pc_ctxlist.pc_nctx = 0; \
    for (PCI = (pc) -> pc_ninfo - 1; PCI >= 0; PCI--) \
	if ((pc) -> pc_info[PCI]) \
	    pe_free ((pc) -> pc_info[PCI]), (pc) -> pc_info[PCI] = NULLPE; \
    (pc) -> pc_ninfo = 0; \
}
    

					/* PRESENTATION requirements */
#define	PR_MANAGEMENT	0x0001	/* context management */
#define	PR_RESTORATION	0x0002	/* context restoration */

#define	PR_MYREQUIRE	0x0000


struct PSAPdata {		/* P-READ.INDICATION */
    int	    px_type;		/* type of indication */
				/* same values as sx_type */

    int	    px_ninfo;		/* number of elements */
    PE	    px_info[NPDATA];	/* data */
};
#define	PXFREE(px) \
{ \
    register int PXI; \
 \
    for (PXI = (px) -> px_ninfo - 1; PXI >= 0; PXI--) \
	if ((px) -> px_info[PXI]) \
	    pe_free ((px) -> px_info[PXI]), (px) -> px_info[PXI] = NULLPE; \
    (px) -> px_ninfo = 0; \
}


struct PSAPtoken {		/* P-{TOKEN-*,GIVE-CONTROL}.INDICATION */
    int	    pt_type;		/* type of indication */
				/* same values as st_type */

    u_char  pt_tokens;		/* tokens offered/wanted */
				/* same values as st_tokens */

    u_char  pt_owned;		/* tokens owned by user */


				/* PLEASE TOKEN only */
    int	    pt_ninfo;		/*   number of elements */
    PE	    pt_info[NPDATA];	/*   data */
};
#define	PTFREE(pt) \
{ \
    register int PTI; \
 \
    for (PTI = (pt) -> pt_ninfo - 1; PTI >= 0; PTI--) \
	if ((pt) -> pt_info[PTI]) \
	    pe_free ((pt) -> pt_info[PTI]), (pt) -> pt_info[PTI] = NULLPE; \
    (pt) -> pt_ninfo = 0; \
}


struct PSAPsync {		/* P-*-SYNC.{INDICATION,CONFIRMATION} */
    int	    pn_type;		/* type of indication/confirmation */
				/* same values as sn_type */

    int	    pn_options;		/* options (!!) */
				/* same values as sn_options */

    long    pn_ssn;		/* serial number */
				/* same values as sn_ssn */

    int	    pn_settings;	/* new token settings (RESYNC only) */

				/* sync data from peer */
    int	    pn_ninfo;		/*   number of elements */
    PE	    pn_info[NPDATA];	/*   data */
};
#define	PNFREE(pn) \
{ \
    register int PNI; \
 \
    for (PNI = (pn) -> pn_ninfo - 1; PNI >= 0; PNI--) \
	if ((pn) -> pn_info[PNI]) \
	    pe_free ((pn) -> pn_info[PNI]), (pn) -> pn_info[PNI] = NULLPE; \
    (pn) -> pn_ninfo = 0; \
}


struct PSAPactivity {		/* P-ACTIVITY-*.{INDICATION,CONFIRMATION} */
    int	    pv_type;		/* type of indication/confirmation */
				/* same values as sv_type */

    struct SSAPactid pv_id;	/* START/RESUME activity identifier */

    struct SSAPactid pv_oid;	/* RESUME old activity identifier */
    struct SSAPref   pv_connect;/* 	  old connection identifier */

    long    pv_ssn;		/* RESUME/END Serial number */

    int	    pv_reason;		/* INTERRUPT/DISCARD */
				/* same values as sp_reason */
        
				/* activity DATA from peer */
    int	    pv_ninfo;		/*   number of elements */
    PE	    pv_info[NPDATA];	/*   data */
};
#define	PVFREE(pv) \
{ \
    register int PVI; \
 \
    for (PVI = (pv) -> pv_ninfo - 1; PVI >= 0; PVI--) \
	if ((pv) -> pv_info[PVI]) \
	    pe_free ((pv) -> pv_info[PVI]), (pv) -> pv_info[PVI] = NULLPE; \
    (pv) -> pv_ninfo = 0; \
}


struct PSAPreport {		/* P-{U,P}-EXCEPTION-REPORT.INDICATION */
    int	    pp_peer;		/* T   = P-U-EXCEPTION-REPORT.INDICATION:
					pp_reason/pp_info both meaningful
				   NIL = P-P-EXCEPTION-REPORT.INDICATION:
					pp_reason == SP_NOREASON, or
					pp_reason == SP_PROTOCOL */
    int	    pp_reason;		/* same values as sp_reason */

				/* report DATA from peer */
    int	    pp_ninfo;		/*   number of elements */
    PE	    pp_info[NPDATA];	/*   data */
};
#define	PPFREE(pp) \
{ \
    register int PPI; \
 \
    for (PPI = (pp) -> pp_ninfo - 1; PPI >= 0; PPI--) \
	if ((pp) -> pp_info[PPI]) \
	    pe_free ((pp) -> pp_info[PPI]), (pp) -> pp_info[PPI] = NULLPE; \
    (pp) -> pp_ninfo = 0; \
}


struct PSAPfinish {		/* P-RELEASE.INDICATION */
				/* release DATA from peer */
    int	    pf_ninfo;		/*   number of elements */
    PE	    pf_info[NPDATA];	/*   data */
};
#define	PFFREE(pf) \
{ \
    register int PFI; \
 \
    for (PFI = (pf) -> pf_ninfo - 1; PFI >= 0; PFI--) \
	if ((pf) -> pf_info[PFI]) \
	    pe_free ((pf) -> pf_info[PFI]), (pf) -> pf_info[PFI] = NULLPE; \
    (pf) -> pf_ninfo = 0; \
}


struct PSAPrelease {		/* P-RELEASE.CONFIRMATION */
    int	    pr_affirmative;	/* T   = connection released
				   NIL = request refused */

				/* release DATA from peer */
    int	    pr_ninfo;		/*   number of elements */
    PE	    pr_info[NPDATA];	/*   data */
};
#define	PRFREE(pr) \
{ \
    register int PRI; \
 \
    for (PRI = (pr) -> pr_ninfo - 1; PRI >= 0; PRI--) \
	if ((pr) -> pr_info[PRI]) \
	    pe_free ((pr) -> pr_info[PRI]), (pr) -> pr_info[PRI] = NULLPE; \
    (pr) -> pr_ninfo = 0; \
}


struct PSAPabort {		/* P-{U,P}-ABORT.INDICATION */
    int	    pa_peer;		/* T   = P-U-ABORT.INDICATION:
					    pa_info/pa_ninfo is meaningful
				   NIL = P-P-ABORT.INDICATION:
					    pa_reason/pa_ppdu is meaningful,
					    pa_data contains diagnostics */

    int	    pa_reason;		/* same codes as pc_result */

				/* abort information from peer */
    int	    pa_ninfo;		/*   number of elements */
    PE	    pa_info[NPDATA];	/*   data */

				/* diagnostics from provider */
#define	PA_SIZE		512
    int	    pa_cc;		/*   length */
    char    pa_data[PA_SIZE];	/*   data */
};
#define	PAFREE(pa) \
{ \
    register int PAI; \
 \
    for (PAI = (pa) -> pa_ninfo - 1; PAI >= 0; PAI--) \
	if ((pa) -> pa_info[PAI]) \
	    pe_free ((pa) -> pa_info[PAI]), (pa) -> pa_info[PAI] = NULLPE; \
    (pa) -> pa_ninfo = 0; \
}


struct PSAPindication {
    int	    pi_type;		/* the union element present */
#define	PI_DATA		0x00
#define	PI_TOKEN	0x01
#define	PI_SYNC		0x02
#define	PI_ACTIVITY	0x03
#define	PI_REPORT	0x04
#define	PI_FINISH	0x05
#define	PI_ABORT	0x06

    union {
	struct PSAPdata pi_un_data;
	struct PSAPtoken pi_un_token;
	struct PSAPsync pi_un_sync;
	struct PSAPactivity pi_un_activity;
	struct PSAPreport pi_un_report;
	struct PSAPfinish pi_un_finish;
	struct PSAPabort pi_un_abort;
    }	pi_un;
#define	pi_data		pi_un.pi_un_data
#define	pi_token	pi_un.pi_un_token
#define	pi_sync		pi_un.pi_un_sync
#define	pi_activity	pi_un.pi_un_activity
#define	pi_report	pi_un.pi_un_report
#define	pi_finish	pi_un.pi_un_finish
#define	pi_abort	pi_un.pi_un_abort
};

/*  */

extern char *psap2version;


int	PExec ();		/* SERVER only */
int	PInit ();		/* P-CONNECT.INDICATION */

int	PConnResponse ();	/* P-CONNECT.RESPONSE */
				/* P-CONNECT.REQUEST (backwards-compatible) */
#define	PConnRequest(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) \
	PAsynConnRequest (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,0)
int	PAsynConnRequest ();	/* P-(ASYN-)CONNECT.REQUEST */
int	PAsynRetryRequest ();	/* P-ASYN-RETRY.REQUEST (pseudo) */
int	PDataRequest ();	/* P-DATA.REQUEST */
int	PDataRequestAux ();	/* P-[*-]DATA.REQUEST */
#define	PPDU_TTD	8
#define	PPDU_TE		9
#define	PPDU_TC	       10
#define	PPDU_TCC       11

#define	PExpdRequest(s,d,n,p) \
	PDataRequestAux ((s), (d), (n), (p), "expedited", SExpdRequest, \
			"SExpdRequest", "P-EXPEDITED-DATA user-data", PPDU_TE)

#define	PTypedRequest(s,d,n,p) \
	PDataRequestAux ((s), (d), (n), (p), "typed", STypedRequest, \
			"STypedRequest", "P-TYPED-DATA user-data", PPDU_TTD)

#define	PCapdRequest(s,d,n,p) \
	PDataRequestAux ((s), (d), (n), (p), "capability", SCapdRequest, \
			"SCapdRequest", "P-CAPABILITY-DATA user-data", PPDU_TC)

#define	PCapdResponse(s,d,n,p) \
	PDataRequestAux ((s), (d), (n), (p), "capability", SCapdResponse, \
			"SCapdResponse","P-CAPABILITY-DATA user-data",PPDU_TCC)

int	PReadRequest ();	/* P-READ.REQUEST (pseudo) */
int	PGTokenRequest ();	/* P-TOKEN-GIVE.REQUEST */
int	PPTokenRequest ();	/* P-TOKEN-PLEASE.REQUEST */
int	PGControlRequest ();	/* P-CONTROL-GIVE.REQUEST */
int	PMajSyncRequestAux ();	/* P-{MAJOR-SYNC,ACTIVITY-END}.REQUEST */

#define	PMajSyncRequest(s,i,d,n,p) \
	PMajSyncRequestAux ((s), (i), (d), (n), (p), "majorsync", \
			SMajSyncRequest, "SMajSyncRequest")

#define	PActEndRequest(s,i,d,n,p) \
	PMajSyncRequestAux ((s), (i), (d), (n), (p), "activity end", \
			SActEndRequest, "SActEndRequest")

int	PMajSyncResponseAux ();	/* P-{MAJOR-SYNC,ACTIVITY-END}.RESPONSE */

#define	PMajSyncResponse(s,d,n,p) \
	PMajSyncResponseAux ((s), (d), (n), (p), "majorsync", \
			SMajSyncResponse, "SMajSyncResponse")

#define	PActEndResponse(s,d,n,p) \
	PMajSyncResponseAux ((s), (d), (n), (p), "activity end", \
			SActEndResponse, "SActEndResponse")

int	PMinSyncRequest ();	/* P-MINOR-SYNC.REQUEST */
int	PMinSyncResponse ();	/* P-MINOR-SYNC.RESPONSE */
int	PReSyncRequest ();	/* P-RESYNCHRONIZE.REQUEST */
int	PReSyncResponse ();	/* P-RESYNCHRONIZE.RESPONSE */
int	PActStartRequest ();	/* P-ACTIVITY-START.REQUEST */
int	PActResumeRequest ();	/* P-ACTIVITY-RESUME.REQUEST */
int	PActIntrRequestAux ();	/* P-ACTIVITY-{INTERRUPT,DISCARD}.REQUEST */

#define	PActIntrRequest(s,r,p) \
	PActIntrRequestAux ((s), (r), (p), \
			SActIntrRequest, "SActIntrRequest")

#define	PActDiscRequest(s,r,p) \
	PActIntrRequestAux ((s), (r), (p), \
			SActDiscRequest, "SActDiscRequest")

int	PActIntrResponseAux ();	/* P-ACTIVITY-{INTERRUPT,DISCARD}.RESPONSE */

#define	PActIntrResponse(s,p) \
	PActIntrResponseAux ((s), (p), \
			SActIntrResponse, "SActIntrResponse")

#define	PActDiscResponse(s,p) \
	PActIntrResponseAux ((s), (p), \
			SActDiscResponse, "SActDiscResponse")

int	PUAbortRequest ();	/* P-U-ABORT.REQUEST */
int	PUReportRequest ();	/* P-U-EXCEPTION-REPORT.REQUEST */
int	PRelRequest ();		/* P-RELEASE.REQUEST */
int	PRelRetryRequest ();	/* P-RELEASE-RETRY.REQUEST (pseudo) */
int	PRelResponse ();	/* P-RELEASE.RESPONSE */

int	PSetIndications ();	/* define vectors for INDICATION events */
int	PSelectMask ();		/* map presentation descriptors for select() */

char   *PErrString ();		/* return PSAP error code in string form */

#define	PLocalHostName	getlocalhost
char   *PLocalHostName ();	/* return name of local host (sigh) */
#endif
